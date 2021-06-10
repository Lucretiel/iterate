use std::cell::Cell;

use syn::{
    visit::{self, Visit},
    ExprAssign, ExprAssignOp, ExprAsync, ExprAwait, ExprBinary, ExprBreak, ExprCall, ExprClosure,
    ExprContinue, ExprForLoop, ExprIndex, ExprLoop, ExprMacro, ExprMethodCall, ExprReturn, ExprTry,
    ExprTryBlock, ExprUnary, ExprWhile, ExprYield, Item, Label, Lifetime,
};

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum IsLazyState {
    // By default we're eager. Eagerness is superseeded by any other state.
    // Eager means we're confident there are no side effects, which means we'd
    // rather have an iterator for size hint
    Eager,

    // If we find anything that seems like it shouldn't be eager- which mostly
    // means method calls & operators- we become lazy. Laziness means we're
    // guessing that side effects might happen, or for whatever reason the
    // expression should be deferred to the relevant call to `next`.
    Lazy,

    // ForceEager is caused by control-flow expressions: break, continue,
    // return, ?, etc. Because they have an impact on the surrounding context
    // & control flow, they should *always* be evaluated eagerly.
    //
    // Once we become eager, there's no reason to traverse further, since the
    // state can't ever go anywhere else
    ForceEager,
}

struct IsLazyContainer {
    state: Cell<IsLazyState>,
}

impl IsLazyContainer {
    fn new(state: IsLazyState) -> Self {
        Self {
            state: Cell::new(state),
        }
    }
    fn new_eager() -> Self {
        Self::new(IsLazyState::Eager)
    }

    fn new_lazy() -> Self {
        Self::new(IsLazyState::Lazy)
    }

    fn set_lazy(&self) {
        if self.state.get() == IsLazyState::Eager {
            self.state.set(IsLazyState::Lazy)
        }
    }

    fn force_eager(&self) {
        self.state.set(IsLazyState::ForceEager)
    }
}

pub struct IsLazyVisitor {
    state: IsLazyContainer,
}

impl IsLazyVisitor {
    pub fn new_eager() -> Self {
        Self {
            state: IsLazyContainer::new_eager(),
        }
    }

    pub fn new_lazy() -> Self {
        Self {
            state: IsLazyContainer::new_lazy(),
        }
    }

    pub fn state(self) -> IsLazyState {
        self.state.state.get()
    }
}

impl<'ast> Visit<'ast> for IsLazyVisitor {
    fn visit_item(&mut self, _: &'ast Item) {
        // Item definitions don't have any bearing on anything; don't descend
        // into them
    }

    fn visit_expr_assign(&mut self, node: &'ast ExprAssign) {
        self.state.set_lazy();
        visit::visit_expr_assign(self, node);
    }

    fn visit_expr_assign_op(&mut self, node: &'ast ExprAssignOp) {
        self.state.set_lazy();
        visit::visit_expr_assign_op(self, node);
    }

    fn visit_expr_async(&mut self, _: &'ast ExprAsync) {
        // Async blocks don't do anything, so no reason to traverse inside
    }

    fn visit_expr_await(&mut self, _: &'ast ExprAwait) {
        // Await is control flow, it must be evaluated eagerly
        self.state.force_eager()
    }

    fn visit_expr_binary(&mut self, node: &'ast ExprBinary) {
        self.state.set_lazy();
        visit::visit_expr_binary(self, node)
    }

    fn visit_expr_break(&mut self, _: &'ast ExprBreak) {
        self.state.force_eager()
    }

    fn visit_expr_call(&mut self, node: &'ast ExprCall) {
        self.state.set_lazy();
        visit::visit_expr_call(self, node)
    }

    fn visit_expr_closure(&mut self, _: &'ast ExprClosure) {
        // Closure expressions don't do anything; don't traverse inside
    }

    fn visit_expr_continue(&mut self, _: &'ast ExprContinue) {
        self.state.force_eager()
    }

    fn visit_expr_for_loop(&mut self, node: &'ast ExprForLoop) {
        self.state.set_lazy();

        let mut visitor = IsLazyLoopVisitor::Base {
            state: &self.state,
            label: node.label.as_ref(),
        };

        visit::visit_expr_for_loop(&mut visitor, node)
    }

    fn visit_expr_index(&mut self, node: &'ast ExprIndex) {
        self.state.set_lazy();
        visit::visit_expr_index(self, node)
    }

    fn visit_expr_loop(&mut self, node: &'ast ExprLoop) {
        self.state.set_lazy();

        let mut visitor = IsLazyLoopVisitor::Base {
            state: &self.state,
            label: node.label.as_ref(),
        };

        visit::visit_expr_loop(&mut visitor, node)
    }

    fn visit_expr_macro(&mut self, node: &'ast ExprMacro) {
        self.state.set_lazy();
        visit::visit_expr_macro(self, node);
    }

    fn visit_expr_method_call(&mut self, node: &'ast ExprMethodCall) {
        self.state.set_lazy();
        visit::visit_expr_method_call(self, node)
    }

    fn visit_expr_return(&mut self, _: &'ast ExprReturn) {
        self.state.force_eager();
    }

    fn visit_expr_try(&mut self, _: &'ast ExprTry) {
        self.state.force_eager();
    }

    fn visit_expr_try_block(&mut self, _: &'ast ExprTryBlock) {
        panic!("Try blocks aren't supported yet");
    }

    fn visit_expr_unary(&mut self, node: &'ast ExprUnary) {
        self.state.set_lazy();
        visit::visit_expr_unary(self, node);
    }

    fn visit_expr_while(&mut self, node: &'ast ExprWhile) {
        self.state.set_lazy();

        let mut visitor = IsLazyLoopVisitor::Base {
            state: &self.state,
            label: node.label.as_ref(),
        };

        visit::visit_expr_while(&mut visitor, node)
    }

    fn visit_expr_yield(&mut self, _: &'ast ExprYield) {
        self.state.force_eager();
    }
}

enum IsLazyLoopVisitor<'ast, 'a> {
    Base {
        label: Option<&'ast Label>,
        state: &'a IsLazyContainer,
    },
    Child {
        label: Option<&'ast Label>,
        parent: &'a IsLazyLoopVisitor<'ast, 'a>,
    },
}

impl<'ast, 'a> IsLazyLoopVisitor<'ast, 'a> {
    fn state(&self) -> &IsLazyContainer {
        match *self {
            IsLazyLoopVisitor::Base { state, .. } => state,
            IsLazyLoopVisitor::Child { parent, .. } => parent.state(),
        }
    }

    fn label(&self) -> Option<&'ast Label> {
        match *self {
            IsLazyLoopVisitor::Base { label, .. } | IsLazyLoopVisitor::Child { label, .. } => label,
        }
    }

    fn has_label(&self, target: &'ast Lifetime) -> bool {
        match self.label() {
            Some(label) if label.name == *target => true,
            _ => match *self {
                IsLazyLoopVisitor::Base { .. } => false,
                IsLazyLoopVisitor::Child { parent, .. } => parent.has_label(target),
            },
        }
    }
}

impl<'ast, 'a> Visit<'ast> for IsLazyLoopVisitor<'ast, 'a> {
    fn visit_item(&mut self, _: &'ast Item) {
        // Item definitions don't have any bearing on anything; don't descend
        // into them
    }

    fn visit_expr_async(&mut self, _: &'ast ExprAsync) {
        // Async blocks don't do anything, so no reason to traverse inside
    }

    fn visit_expr_await(&mut self, _: &'ast ExprAwait) {
        // Await is control flow, it must be evaluated eagerly
        self.state().force_eager()
    }

    fn visit_expr_break(&mut self, node: &'ast ExprBreak) {
        match node.label {
            Some(ref label) if !self.has_label(label) => self.state().force_eager(),
            _ => visit::visit_expr_break(self, node),
        }
    }

    fn visit_expr_closure(&mut self, _: &'ast ExprClosure) {
        // Closure expressions don't do anything; don't traverse inside
    }

    fn visit_expr_continue(&mut self, node: &'ast ExprContinue) {
        match node.label {
            Some(ref label) if !self.has_label(label) => self.state().force_eager(),
            _ => visit::visit_expr_continue(self, node),
        }
    }

    fn visit_expr_for_loop<'b>(&'b mut self, node: &'ast ExprForLoop) {
        let mut visitor = IsLazyLoopVisitor::Child {
            parent: self,
            label: node.label.as_ref(),
        };

        visit::visit_expr_for_loop(&mut visitor, node)
    }

    fn visit_expr_loop(&mut self, node: &'ast ExprLoop) {
        let mut visitor = IsLazyLoopVisitor::Child {
            parent: self,
            label: node.label.as_ref(),
        };

        visit::visit_expr_loop(&mut visitor, node)
    }

    fn visit_expr_return(&mut self, _: &'ast ExprReturn) {
        self.state().force_eager();
    }

    fn visit_expr_try(&mut self, _: &'ast ExprTry) {
        self.state().force_eager();
    }

    fn visit_expr_try_block(&mut self, _: &'ast ExprTryBlock) {
        panic!("Try blocks aren't supported yet");
    }

    fn visit_expr_while(&mut self, node: &'ast ExprWhile) {
        let mut visitor = IsLazyLoopVisitor::Child {
            parent: self,
            label: node.label.as_ref(),
        };

        visit::visit_expr_while(&mut visitor, node)
    }

    fn visit_expr_yield(&mut self, _: &'ast ExprYield) {
        self.state().force_eager();
    }
}
