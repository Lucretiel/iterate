use std::{cmp::Ordering, iter};

use itertools::Itertools;
use joinery::{prelude::*, separators::Comma};
use proc_macro::TokenStream;
use proc_macro2::{Ident, Span, TokenStream as TokenStream2};
use quote::{format_ident, quote};
use syn::{
    parse::{Parse, ParseStream, Parser},
    punctuated::Punctuated,
    Expr, Token,
};

enum GenerateDescriptor {
    Item {
        /// The identifier in the struct
        field: Ident,

        /// The generic type in the struct
        ty: Ident,

        /// The name of the variant
        variant: Ident,

        /// The expression to actually use when creating the iterator
        expr: Expr,
    },
    Iter {
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

#[derive(Clone, Copy)]
enum StateVariant<'a> {
    Item {
        field: &'a Ident,
        variant: &'a Ident,
    },
    Iter {
        field: &'a Ident,
        base_variant: &'a Ident,
        iter_variant: &'a Ident,
        variant_ty: &'a Ident,
    },
    Dead {
        variant: &'a Ident,
    },
}

impl<'a> StateVariant<'a> {
    fn base_variant(&self) -> &'a Ident {
        match *self {
            StateVariant::Item { variant, .. } => variant,
            StateVariant::Iter { base_variant, .. } => base_variant,
            StateVariant::Dead { variant } => variant,
        }
    }
}

impl<'a> From<&'a GenerateDescriptor> for StateVariant<'a> {
    fn from(desc: &'a GenerateDescriptor) -> Self {
        match desc {
            GenerateDescriptor::Item { field, variant, .. } => {
                StateVariant::Item { field, variant }
            }
            GenerateDescriptor::Iter {
                field,
                base_variant,
                iter_variant,
                variant_ty,
                ..
            } => StateVariant::Iter {
                field,
                base_variant,
                iter_variant,
                variant_ty,
            },
        }
    }
}

enum GenerateItem {
    Item(Expr),
    Unroll(Expr),
}

impl Parse for GenerateItem {
    fn parse(input: ParseStream) -> syn::Result<Self> {
        if input.peek(Token![..]) {
            let _dots: Token![..] = input.parse()?;
            input.parse().map(GenerateItem::Unroll)
        } else {
            input.parse().map(GenerateItem::Item)
        }
    }
}

fn generate_impl(tokens: TokenStream2) -> syn::Result<TokenStream2> {
    let items: Punctuated<GenerateItem, Token![,]> = Punctuated::parse_terminated.parse2(tokens)?;

    let descriptors = items
        .into_iter()
        .enumerate()
        .map(|(i, item)| match item {
            GenerateItem::Item(expr) => GenerateDescriptor::Item {
                field: format_ident!("item{}", i, span = Span::mixed_site()),
                ty: format_ident!("Item{}", i, span = Span::mixed_site()),
                variant: format_ident!("Item{}", i, span = Span::mixed_site()),
                expr,
            },
            GenerateItem::Unroll(expr) => GenerateDescriptor::Iter {
                field: format_ident!("iter{}", i, span = Span::mixed_site()),
                lazy_ty: format_ident!("IterFunc{}", i, span = Span::mixed_site()),
                iter_ty: format_ident!("Iter{}", i, span = Span::mixed_site()),
                base_variant: format_ident!("StartIter{}", i, span = Span::mixed_site()),
                iter_variant: format_ident!("Iter{}", i, span = Span::mixed_site()),
                variant_ty: format_ident!("Iter{}", i, span = Span::mixed_site()),
                expr,
            },
        })
        .collect_vec();

    let dead_ident = Ident::new("Dead", Span::mixed_site());
    let state_ident = Ident::new("GenerateIterState", Span::mixed_site());
    let iter_ident = Ident::new("GenerateIter", Span::mixed_site());
    let item_ident = Ident::new("T", Span::mixed_site());

    // The set of generics used by the iterator struct type
    let iter_generics = descriptors
        .iter()
        .map(|desc| match desc {
            GenerateDescriptor::Item { ty, .. } => quote! { #ty },
            GenerateDescriptor::Iter {
                lazy_ty, iter_ty, ..
            } => quote! { #lazy_ty, #iter_ty },
        })
        .join_with(Comma);

    // The set of generics used by the iterator struct type with type bounds
    // attached
    let iter_generic_bounds = descriptors
        .iter()
        .map(|desc| match desc {
            GenerateDescriptor::Item { ty, .. } => quote! {
                #ty: FnOnce() -> #item_ident
            },
            GenerateDescriptor::Iter {
                lazy_ty, iter_ty, ..
            } => quote! {
                #lazy_ty: FnOnce() -> #iter_ty,
                #iter_ty: IntoIterator<Item=#item_ident>
            },
        })
        .join_with(Comma);

    // The set of data fields in the iterator struct. The actual struct
    // also includes a phantom data, head, and tail.
    let iter_fields = descriptors
        .iter()
        .map(|desc| match desc {
            GenerateDescriptor::Item { field, ty, .. } => quote! {
                #field: ManuallyDrop<#ty>
            },
            GenerateDescriptor::Iter { field, lazy_ty, .. } => quote! {
                #field: ManuallyDrop<#lazy_ty>
            },
        })
        .join_with(Comma);

    // The set of generics used by the state enum, used in the definition
    let state_generics = descriptors
        .iter()
        .filter_map(|desc| match desc {
            GenerateDescriptor::Iter { variant_ty, .. } => Some(variant_ty),
            _ => None,
        })
        .join_with(Comma);

    // The set of type arguments applied to the state enum
    let state_in_struct_generics = descriptors
        .iter()
        .filter_map(|desc| match desc {
            GenerateDescriptor::Iter { iter_ty, .. } => {
                Some(quote! {<#iter_ty as IntoIterator>::IntoIter})
            }
            _ => None,
        })
        .join_with(Comma);

    // All of the variants of the state enum, including the trailing dead variant.
    let variants = descriptors
        .iter()
        .map(StateVariant::from)
        .chain(iter::once_with(|| StateVariant::Dead {
            variant: &dead_ident,
        }));

    // The set of variants for the state enum
    let state_variants = variants
        .clone()
        .map(|variant| match variant {
            StateVariant::Item { variant, .. } | StateVariant::Dead { variant } => quote! {
                #variant
            },
            StateVariant::Iter {
                base_variant,
                iter_variant,
                variant_ty,
                ..
            } => quote! {
                #base_variant,
                #iter_variant(#variant_ty)
            },
        })
        .join_with(Comma);

    let next_branch_arms = {
        let next_idents = variants
            .clone()
            .skip(1)
            .map(|desc| desc.base_variant())
            .chain(iter::repeat_with(|| &dead_ident));

        variants
            .clone()
            .zip(next_idents)
            .map(|(variant, next_variant)| match variant {
                StateVariant::Item { field, variant } => quote! {
                    #state_ident::#variant => break (
                        #state_ident::#next_variant,
                        unsafe { ManuallyDrop::take(&mut self.#field) }(),
                    )
                },
                StateVariant::Iter {
                    field,
                    base_variant,
                    iter_variant,
                    ..
                } => quote! {
                    #state_ident::#base_variant => #state_ident::#iter_variant(
                        unsafe { ManuallyDrop::take(&mut self.#field) }().into_iter()
                    ),
                    #state_ident::#iter_variant(mut iter) => match iter.next() {
                        None => #state_ident::#next_variant,
                        Some(item) => break (#state_ident::#iter_variant(iter), item),
                    }
                },
                StateVariant::Dead { variant } => quote! {
                    #state_ident::#variant => return None
                },
            })
            .join_with(Comma)
    };

    // Indices are used to check for before/after relationships between
    // variants. This is used to implement double ended iteration and drop.
    let idx_branch_arms = variants
        .clone()
        .scan(0usize, |state, variant| match variant {
            StateVariant::Item { variant, .. } => {
                let idx = *state;
                *state += 1;

                Some(quote! { #state_ident::#variant => #idx })
            }
            StateVariant::Iter {
                base_variant,
                iter_variant,
                ..
            } => {
                let base_idx = *state;
                let iter_idx = *state + 1;
                *state += 2;

                Some(quote! {
                    #state_ident::#base_variant => #base_idx,
                    #state_ident::#iter_variant(..) => #iter_idx
                })
            }
            StateVariant::Dead { variant } => {
                let idx = *state;
                *state += 1;

                Some(quote! { #state_ident::#variant => #idx })
            }
        });

    // Distances are used when computing size_hint. Given a head and a tail,
    // the difference between their distances is the number of single elements
    // between them. Iter elements could have 0 items, so they're not used.
    let distance_branch_arms = variants
        .clone()
        .scan(0usize, |state, variant| match variant {
            StateVariant::Item { variant, .. } => {
                let distance = *state;
                *state += 1;

                Some(quote! { #state_ident::#variant => #distance })
            }
            StateVariant::Iter {
                base_variant,
                iter_variant,
                ..
            } => Some(quote! {
                #state_ident::#base_variant => #state,
                #state_ident::#iter_variant(..) => #state
            }),
            StateVariant::Dead { variant } => {
                let distance = *state;
                *state += 1;

                Some(quote! { #state_ident::#variant => #distance })
            }
        });

    let init_exprs = descriptors
        .iter()
        .map(|desc| match desc {
            GenerateDescriptor::Item { field, expr, .. } => quote! {
                #field: ManuallyDrop::new(move || #expr)
            },
            GenerateDescriptor::Iter { field, expr, .. } => quote! {
                #field: ManuallyDrop::new(move || #expr)
            },
        })
        .join_with(Comma);

    let first_variant = variants.clone().next().unwrap().base_variant();
    let last_variant = &dead_ident;

    Ok(quote! {{
        use ::core::{
            marker::PhantomData,
            mem::{ManuallyDrop, self},
            ops::FnOnce,
        };

        // State Enum
        enum #state_ident<#state_generics> {
            #state_variants,
        }

        // Iterator Struct
        struct #iter_ident<#item_ident, #iter_generic_bounds> {
            phantom: PhantomData<#item_ident>,

            head: #state_ident<#state_in_struct_generics>,
            tail: #state_ident<#state_in_struct_generics>,

            #iter_fields,
        }

        impl<#item_ident, #iter_generics> Iterator for #iter_ident<#item_ident, #iter_generics>
        where #iter_generic_bounds
        {
            type Item = #item_ident;

            fn next(&mut self) -> Option<Self::Item> {
                let (state, next) = loop {
                    let state = match mem::replace(&mut self.head, #state_ident::Dead) {
                        #next_branch_arms,
                    };
                    self.head = state;
                };
                self.head = state;
                Some(next)
            }
        }

        // Iterator impls (Iterator + size_hint, ReverseIterator, Fuse, Clone, Debug, Drop)
        #iter_ident {
            phantom: PhantomData,

            head: #state_ident::#first_variant,
            tail: #state_ident::#last_variant,

            #init_exprs,
        }
    }})
}

#[proc_macro]
pub fn generate(input: TokenStream) -> TokenStream {
    match generate_impl(input.into()) {
        Ok(tokens) => tokens.into(),
        Err(err) => err.into_compile_error().into(),
    }
}
