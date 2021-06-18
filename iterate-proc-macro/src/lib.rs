/*!
Support crate for `iterate`. Do not depend directly on this crate.
*/

mod visitor;

use std::{array, iter};

use either::Either;
use itertools::Itertools;
use joinery::{prelude::*, separators::Comma};
use proc_macro::TokenStream;
use proc_macro2::{Ident, Span, TokenStream as TokenStream2};
use quote::{format_ident, quote};
use syn::{
    parse::{Parse, ParseStream, Parser},
    punctuated::Punctuated,
    visit::Visit,
    Expr, Token,
};

use visitor::{IsLazyState, IsLazyVisitor};

enum GenerateDescriptor<'a> {
    EagerItem {
        /// The identifier in the struct
        field: Ident,

        /// The generic type in the struct
        ty: &'a Ident,

        /// The name of the variant
        variant: Ident,

        /// The expression to actually use when creating the iterator
        expr: Expr,
    },
    LazyItem {
        /// The identifier in the struct
        field: Ident,

        /// The generic type in the struct
        ty: Ident,

        /// The name of the variant
        variant: Ident,

        /// The expression to actually use when creating the iterator
        expr: Expr,
    },
    EagerIter {
        /// The identifier in the struct
        field: Ident,

        /// The iterator type
        ty: Ident,

        /// The name of the variant
        variant: Ident,

        /// The expression to use when creating the iterator
        expr: Expr,
    },
    LazyIter {
        /// The identifier in the struct
        field: Ident,

        /// The type of the field in the struct
        lazy_ty: Ident,

        /// The type of the IntoIter in the struct
        iter_ty: Ident,

        /// The name of the variant before the iterator has started
        base_variant: Ident,

        /// The name of the variant containing the iterator
        iter_variant: Ident,

        /// The type of the Iterator in the variant
        variant_ty: Ident,

        /// The expression to actually use when creating the iterator
        expr: Expr,
    },
}

impl GenerateDescriptor<'_> {
    fn field(&self) -> &Ident {
        match self {
            GenerateDescriptor::EagerItem { field, .. }
            | GenerateDescriptor::LazyItem { field, .. }
            | GenerateDescriptor::LazyIter { field, .. }
            | GenerateDescriptor::EagerIter { field, .. } => field,
        }
    }

    fn field_ty(&self) -> &Ident {
        match self {
            GenerateDescriptor::EagerItem { ty, .. } => *ty,
            GenerateDescriptor::LazyItem { ty, .. }
            | GenerateDescriptor::LazyIter { lazy_ty: ty, .. }
            | GenerateDescriptor::EagerIter { ty, .. } => ty,
        }
    }
}

#[derive(Debug, Clone, Copy)]
enum StateVariant<'a> {
    EagerItem {
        field: &'a Ident,
        variant: &'a Ident,
    },
    LazyItem {
        field: &'a Ident,
        variant: &'a Ident,
    },
    EagerIter {
        field: &'a Ident,
        variant: &'a Ident,
    },
    BeginIter {
        field: &'a Ident,
        variant: &'a Ident,
    },
    Iter {
        variant: &'a Ident,
        variant_ty: &'a Ident,
    },
    Dead {
        variant: &'a Ident,
    },
}

impl<'a> StateVariant<'a> {
    fn ident(&self) -> &'a Ident {
        match *self {
            StateVariant::EagerItem { variant, .. }
            | StateVariant::LazyItem { variant, .. }
            | StateVariant::EagerIter { variant, .. }
            | StateVariant::BeginIter { variant, .. }
            | StateVariant::Iter { variant, .. }
            | StateVariant::Dead { variant } => variant,
        }
    }

    fn is_iter(&self) -> bool {
        matches!(*self, StateVariant::Iter { .. })
    }
}

#[derive(Debug, Clone)]
struct VariantList<'a> {
    variants: Vec<StateVariant<'a>>,
    dead_ident: &'a Ident,
}

impl<'a> VariantList<'a> {
    fn build(
        descriptors: impl IntoIterator<Item = &'a GenerateDescriptor<'a>>,
        dead_ident: &'a Ident,
    ) -> Self {
        let variants = descriptors
            .into_iter()
            .flat_map(|descriptor| match descriptor {
                GenerateDescriptor::EagerItem { field, variant, .. } => {
                    Either::Left(iter::once(StateVariant::EagerItem { field, variant }))
                }
                GenerateDescriptor::LazyItem { field, variant, .. } => {
                    Either::Left(iter::once(StateVariant::LazyItem { field, variant }))
                }
                GenerateDescriptor::EagerIter { field, variant, .. } => {
                    Either::Left(iter::once(StateVariant::EagerIter { field, variant }))
                }
                GenerateDescriptor::LazyIter {
                    field,
                    base_variant,
                    iter_variant,
                    variant_ty,
                    ..
                } => Either::Right(array::IntoIter::new([
                    StateVariant::BeginIter {
                        field,
                        variant: base_variant,
                    },
                    StateVariant::Iter {
                        variant: iter_variant,
                        variant_ty,
                    },
                ])),
            })
            .collect();

        Self {
            variants,
            dead_ident,
        }
    }

    /// Produce a list of all the variants in the state
    fn iter(&self) -> impl Iterator<Item = StateVariant<'a>> + Clone + '_ {
        self.variants
            .iter()
            .copied()
            .chain(iter::once(StateVariant::Dead {
                variant: self.dead_ident,
            }))
    }

    /// Get the first variant in the sequence. This is guaranteed to be a unit
    /// ident.
    fn first_variant(&self) -> &'a Ident {
        match self.variants.first() {
            Some(variant) => variant.ident(),
            None => self.dead_ident,
        }
    }

    // Given the index of a variant, return the identifier of the next variant
    // in the sequence
    fn next_ident(&self, variant_idx: usize) -> &'a Ident {
        match self.variants.get(variant_idx.saturating_add(1)) {
            None => self.dead_ident,
            Some(variant) => variant.ident(),
        }
    }

    // Given the index of a variant, return the identifier of the next non-iter
    // variant in the sequence. Used in drop to skip over Iter variants.
    fn next_unit_ident(&self, variant_idx: usize) -> &'a Ident {
        let idx = variant_idx.saturating_add(1);

        match self.variants.get(idx) {
            None => self.dead_ident,
            Some(variant) if !variant.is_iter() => variant.ident(),
            // It's guaranteed that an iter variant is followed by a non iter
            // variant
            Some(..) => self.next_ident(idx),
        }
    }
}

enum GenerateItem {
    LazyItem(Expr),
    EagerItem(Expr),
    EagerIter(Expr),
    LazyIter(Expr),
}

impl Parse for GenerateItem {
    fn parse(input: ParseStream) -> syn::Result<Self> {
        if input.peek(Token![..]) {
            let _dots: Token![..] = input.parse()?;

            input.parse().map(|expr| {
                let mut visitor = IsLazyVisitor::new_eager();
                visitor.visit_expr(&expr);
                match visitor.state() {
                    IsLazyState::Eager => GenerateItem::EagerIter(expr),
                    IsLazyState::Lazy => GenerateItem::LazyIter(expr),
                    IsLazyState::ForceEager => GenerateItem::EagerIter(expr),
                }
            })
        } else {
            input.parse().map(|expr| {
                let mut visitor = IsLazyVisitor::new_lazy();
                visitor.visit_expr(&expr);
                match visitor.state() {
                    IsLazyState::Eager => GenerateItem::EagerItem(expr),
                    IsLazyState::Lazy => GenerateItem::LazyItem(expr),
                    IsLazyState::ForceEager => GenerateItem::EagerItem(expr),
                }
            })
        }
    }
}

fn generate_impl(tokens: TokenStream2) -> syn::Result<TokenStream2> {
    let mut iter = tokens.into_iter();
    let t1 = iter.next().expect("the crate token");
    let iterate_crate = syn::parse2::<syn::Ident>(t1.into())?;
    let t2 = iter.next().expect("the unsafe token");
    let _ = syn::parse2::<Token![unsafe]>(t2.into())?;
    let t3 = iter.next().expect("the rest of the args");
    let tokens = match t3 {
        proc_macro2::TokenTree::Group(g) => { g.stream() }
        other => { panic!("unexpected 3rd arg {:?}", other); }
    };

    let items: Punctuated<GenerateItem, Token![,]> = Punctuated::parse_terminated.parse2(tokens)?;

    let dead_ident = Ident::new("Dead", Span::mixed_site());
    let state_ident = Ident::new("LocalIterateState", Span::mixed_site());
    let iter_ident = Ident::new("LocalIterate", Span::mixed_site());
    let item_ident = Ident::new("T", Span::mixed_site());

    // Used for size_hint implementation
    let size_hint_ident = Ident::new("size_hint", Span::mixed_site());
    let idx_ident = Ident::new("idx", Span::mixed_site());

    let descriptors = items
        .into_iter()
        .enumerate()
        .map(|(i, item)| match item {
            GenerateItem::EagerItem(expr) => GenerateDescriptor::EagerItem {
                field: format_ident!("eager_item{}", i, span = Span::mixed_site()),
                ty: &item_ident,
                variant: format_ident!("StateItem{}", i, span = Span::mixed_site()),
                expr,
            },
            GenerateItem::LazyItem(expr) => GenerateDescriptor::LazyItem {
                field: format_ident!("lazy_item{}", i, span = Span::mixed_site()),
                ty: format_ident!("Item{}", i, span = Span::mixed_site()),
                variant: format_ident!("StateItem{}", i, span = Span::mixed_site()),
                expr,
            },
            GenerateItem::EagerIter(expr) => GenerateDescriptor::EagerIter {
                field: format_ident!("eager_iter{}", i, span = Span::mixed_site()),
                ty: format_ident!("Iter{}", i, span = Span::mixed_site()),
                variant: format_ident!("StateIter{}", i, span = Span::mixed_site()),
                expr,
            },
            GenerateItem::LazyIter(expr) => GenerateDescriptor::LazyIter {
                field: format_ident!("lazy_iter{}", i, span = Span::mixed_site()),
                lazy_ty: format_ident!("IterFunc{}", i, span = Span::mixed_site()),
                iter_ty: format_ident!("Iter{}", i, span = Span::mixed_site()),
                base_variant: format_ident!("StateBeginIter{}", i, span = Span::mixed_site()),
                iter_variant: format_ident!("StateIter{}", i, span = Span::mixed_site()),
                variant_ty: format_ident!("Iter{}", i, span = Span::mixed_site()),
                expr,
            },
        })
        .collect_vec();

    // The set of generics used by the iterator struct type
    let iter_generics = descriptors
        .iter()
        .filter_map(|desc| match desc {
            GenerateDescriptor::EagerItem { .. } => None,
            GenerateDescriptor::LazyItem { ty, .. } => Some(quote! { #ty }),
            GenerateDescriptor::EagerIter { ty, .. } => Some(quote! { #ty }),
            GenerateDescriptor::LazyIter {
                lazy_ty, iter_ty, ..
            } => Some(quote! { #lazy_ty, #iter_ty }),
        })
        .join_with(Comma);

    // The set of generics used by the iterator struct type with type bounds
    // attached
    let iter_generic_bounds = descriptors
        .iter()
        .filter_map(|desc| match desc {
            GenerateDescriptor::EagerItem { .. } => None,
            GenerateDescriptor::LazyItem { ty, .. } => Some(quote! {
                #ty: FnOnce() -> #item_ident
            }),
            GenerateDescriptor::EagerIter { ty, .. } => Some(quote! {
                #ty: Iterator<Item=#item_ident>
            }),
            GenerateDescriptor::LazyIter {
                lazy_ty, iter_ty, ..
            } => Some(quote! {
                #lazy_ty: FnOnce() -> #iter_ty,
                #iter_ty: Iterator<Item=#item_ident>
            }),
        })
        .join_with(Comma);

    // The set of data fields in the iterator struct. The actual struct
    // also includes a phantom data, head, and tail.
    let iter_fields = descriptors.iter().map(|desc| {
        let field = desc.field();
        let ty = desc.field_ty();

        quote! { #field: MaybeUninit<#ty> }
    });

    // The set of generics used by the state enum, used in the definition
    let state_generics = descriptors.iter().filter_map(|desc| match desc {
        GenerateDescriptor::LazyIter { variant_ty, .. } => Some(variant_ty),
        _ => None,
    });

    // The set of type arguments applied to the state enum
    let state_in_struct_generics = descriptors.iter().filter_map(|desc| match desc {
        GenerateDescriptor::LazyIter { iter_ty, .. } => Some(iter_ty),
        _ => None,
    });

    // All of the variants of the state enum, including the trailing dead variant.
    let variants = VariantList::build(&descriptors, &dead_ident);

    // The set of variants for the state enum
    let state_variants = variants.iter().map(|variant| match variant {
        StateVariant::EagerItem { variant, .. }
        | StateVariant::LazyItem { variant, .. }
        | StateVariant::EagerIter { variant, .. }
        | StateVariant::BeginIter { variant, .. }
        | StateVariant::Dead { variant } => quote! {
            #variant
        },
        StateVariant::Iter {
            variant,
            variant_ty,
        } => quote! {
           #variant(#variant_ty)
        },
    });

    let next_branch_arms = variants
        .iter()
        .enumerate()
        // for each variant, get the next identifier in the sequence
        .map(|(idx, variant)| (variant, variants.next_ident(idx)))
        .map(|(variant, next_variant)| match variant {
            StateVariant::EagerItem { field, variant } => quote! {
                #state_ident::#variant => break (
                    #state_ident::#next_variant,
                    unsafe { self.#field.as_mut_ptr().read() },
                )
            },
            StateVariant::LazyItem { field, variant } => quote! {
                #state_ident::#variant => break (
                    #state_ident::#next_variant,
                    unsafe { self.#field.as_mut_ptr().read() }(),
                )
            },
            StateVariant::EagerIter { variant, field } => quote! {
                #state_ident::#variant => match {
                    // Use a nested block here to make sure that the reference
                    // is dropped before the ptr read later
                    let iter = unsafe { &mut *self.#field.as_mut_ptr() };
                    iter.next()
                } {
                    None => {
                        mem::drop(unsafe { self.#field.as_mut_ptr().read() });
                        #state_ident::#next_variant
                    }
                    Some(item) => break (#state_ident::#variant, item),
                }
            },
            StateVariant::BeginIter { variant, field } => quote! {
                #state_ident::#variant => #state_ident::#next_variant(
                    unsafe { self.#field.as_mut_ptr().read() }()
                )
            },
            StateVariant::Iter { variant, .. } => quote! {
                #state_ident::#variant(mut iter) => match iter.next() {
                    None => #state_ident::#next_variant,
                    Some(item) => break (#state_ident::#variant(iter), item),
                }
            },
            StateVariant::Dead { variant } => quote! {
                #state_ident::#variant => return None
            },
        });

    let begin_size_hint_branch_arms =
        variants
            .iter()
            .enumerate()
            .map(|(idx, variant)| match variant {
                StateVariant::EagerItem { variant, .. }
                | StateVariant::LazyItem { variant, .. } => quote! {
                    #state_ident::#variant => (#iterate_crate::exact_size_hint(1), #idx)
                },
                StateVariant::EagerIter { variant, field } => quote! {
                    #state_ident::#variant => {
                        let iter = unsafe { & *self.#field.as_ptr() };
                        (iter.size_hint(), #idx)
                    }
                },
                StateVariant::BeginIter { variant, .. } => quote! {
                    #state_ident::#variant => (#iterate_crate::any_size_hint(), #idx)
                },
                StateVariant::Iter { variant, .. } => quote! {
                    #state_ident::#variant(ref iter) => (iter.size_hint(), #idx)
                },
                StateVariant::Dead { variant } => quote! {
                    #state_ident::#variant => return (#iterate_crate::exact_size_hint(0))
                },
            });

    let finish_size_hint_blocks =
        variants
            .iter()
            .enumerate()
            .skip(1)
            .filter_map(|(idx, variant)| match variant {
                StateVariant::EagerItem { .. } | StateVariant::LazyItem { .. } => Some(quote! {
                    if #idx_ident < #idx {
                        #size_hint_ident = #iterate_crate::add_size_hints(
                            #size_hint_ident,
                            #iterate_crate::exact_size_hint(1),
                        );
                    }
                }),
                StateVariant::EagerIter { field, .. } => Some(quote! {
                    if #idx_ident < #idx {
                        let iter = unsafe { & *self.#field.as_ptr() };
                        #size_hint_ident = #iterate_crate::add_size_hints(
                            #size_hint_ident,
                            iter.size_hint(),
                        );
                    }
                }),
                StateVariant::BeginIter { .. }
                | StateVariant::Iter { .. }
                | StateVariant::Dead { .. } => None,
            });

    let drop_branch_arms = variants
        .iter()
        .enumerate()
        // for each variant, get the next non-iter identifier in the sequence
        .map(|(idx, variant)| (variant, variants.next_unit_ident(idx)))
        .map(|(variant, next_variant)| match variant {
            StateVariant::EagerItem { field, variant }
            | StateVariant::LazyItem { field, variant }
            | StateVariant::BeginIter { field, variant }
            | StateVariant::EagerIter { field, variant } => {
                quote! {
                    #state_ident::#variant => {
                        mem::drop(unsafe { self.#field.as_mut_ptr().read() });
                        #state_ident::#next_variant
                    }
                }
            }
            StateVariant::Iter { variant, .. } => quote! {
                // No additional work needed
                #state_ident::#variant(..) => #state_ident::#next_variant
            },
            StateVariant::Dead { variant } => quote! {
                #state_ident::#variant => break
            },
        });

    let init_exprs = descriptors.iter().map(|desc| match desc {
        GenerateDescriptor::EagerItem { field, expr, .. } => quote! {
            #field: MaybeUninit::new(#expr)
        },
        GenerateDescriptor::EagerIter { field, expr, .. } => quote! {
            #field: MaybeUninit::new(::core::iter::IntoIterator::into_iter(#expr))
        },
        GenerateDescriptor::LazyItem { field, expr, .. } => quote! {
            #field: MaybeUninit::new(move || #expr)
        },
        GenerateDescriptor::LazyIter { field, expr, .. } => quote! {
            #field: MaybeUninit::new(move || ::core::iter::IntoIterator::into_iter(#expr))
        },
    });

    let first_variant = variants.first_variant();

    Ok(quote! {{
        use ::core::{
            marker::PhantomData,
            mem::{MaybeUninit, self},
            ops::FnOnce,
        };

        // State Enum
        enum #state_ident<#(#state_generics),*> {
            #(#state_variants,)*
        }

        // Iterator Struct
        // TODO: The `generate` library should provide a wrapper struct, so
        // that there's no chance of leaking implementation details
        struct #iter_ident<#item_ident, #iter_generic_bounds,> {
            phantom: PhantomData<#item_ident>,

            head: #state_ident<#(#state_in_struct_generics),*>,
            // tail: state.

            #(#iter_fields,)*
        }

        impl<#item_ident, #iter_generics,> Iterator for #iter_ident<#item_ident, #iter_generics,>
        where #iter_generic_bounds,
        {
            type Item = #item_ident;

            fn next(&mut self) -> Option<Self::Item> {
                let (state, next) = loop {
                    let state = match mem::replace(&mut self.head, #state_ident::Dead) {
                        #(#next_branch_arms,)*
                    };
                    self.head = state;
                };
                self.head = state;
                Some(next)
            }

            fn size_hint(&self) -> (usize, Option<usize>) {
                let (mut #size_hint_ident, #idx_ident) = match self.head {
                    #(#begin_size_hint_branch_arms,)*
                };

                #(#finish_size_hint_blocks)*

                #size_hint_ident
            }
        }

        impl<#item_ident, #iter_generics,> ::core::iter::FusedIterator for #iter_ident<#item_ident, #iter_generics,>
        where #iter_generic_bounds,
        {}

        // TODO: ExactSizeIterator
        // TODO: DoubleEndedIterator

        impl<#item_ident, #iter_generics,> Drop for #iter_ident<#item_ident, #iter_generics,>
        where #iter_generic_bounds,
        {
            fn drop(&mut self) {
                loop {
                    self.head = match self.head {
                        #(#drop_branch_arms,)*
                    };
                }
            }
        }

        // Iterator impls (Iterator + size_hint, ReverseIterator, Fuse, Clone, Debug, Drop)
        #iterate_crate::conceal(#iter_ident {
            phantom: PhantomData,

            head: #state_ident::#first_variant,

            #(#init_exprs,)*
        })
    }})
}

#[proc_macro]
pub fn iterate(input: TokenStream) -> TokenStream {
    match generate_impl(input.into()) {
        Ok(tokens) => tokens.into(),
        Err(err) => err.into_compile_error().into(),
    }
}
