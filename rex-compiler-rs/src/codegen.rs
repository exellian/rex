pub mod js {

    pub struct JsCodegen {

    }

    mod implementation {
        use std::collections::HashSet;
        use crate::codegen::js::JsCodegen;
        use crate::rex::parse::{Ap, Attribute, AttributeValue, BinaryAp, BinaryOp, Block, Expr, For, Group, If, Node, NodeOrBlock, Punctuated, SelectorAp, SelectorOp, TagNode, UnaryAp, UnaryOp, Var};
        use crate::rex::parse::primitive::{Comma, Empty, Lit};
        use crate::rex::parse::scope::Scope;
        use crate::rex::parse::typ::Type;
        use crate::View;

        impl JsCodegen {

            pub fn new() -> Self {
                JsCodegen {}
            }

            pub fn escape(str: &str) -> String  {
                str.to_string()
            }

            fn generate_un_op(un_op: &UnaryOp) -> String {
                match un_op {
                    UnaryOp::Neg(_) => "-".to_string(),
                    UnaryOp::Not(_) => "!".to_string()
                }
            }

            fn generate_bin_op(bin_op: &BinaryOp) -> String {
                match bin_op {
                    BinaryOp::Multiplied(_) => "*".to_string(),
                    BinaryOp::Divided(_) => "/".to_string(),
                    BinaryOp::Plus(_) => "+".to_string(),
                    BinaryOp::Minus(_) => "-".to_string(),
                    BinaryOp::Modulo(_) => "%".to_string(),
                    BinaryOp::And(_) => "&&".to_string(),
                    BinaryOp::Or(_) => "||".to_string(),
                    BinaryOp::BitAnd(_) => "&".to_string(),
                    BinaryOp::BitOr(_) => "|".to_string(),
                    BinaryOp::BitXor(_) => "^".to_string(),
                    BinaryOp::Eq(_) => "==".to_string(),
                    BinaryOp::Ne(_) => "!=".to_string(),
                    BinaryOp::Le(_) => "<=".to_string(),
                    BinaryOp::Ge(_) => ">=".to_string(),
                    BinaryOp::Lt(_) => "<".to_string(),
                    BinaryOp::Gt(_) => ">".to_string()
                }
            }

            fn generate_punctuated_expr(mut punct: &Punctuated<Expr, Comma>) -> String {
                let mut str = String::new();
                let e = Self::generate_expr(&punct.expr);
                str.push_str(&e);
                let mut other = &punct.other;
                while let Some((_, o)) = other {
                    let e = Self::generate_expr(&o.expr);
                    str.push_str(",");
                    str.push_str(&e);
                    other = &o.other
                }
                str
            }

            fn generate_if(expr: &If) -> String {
                let mut str = String::new();
                let mut inner = String::new();
                let mut condition = Self::generate_expr(&expr.condition);
                let mut if_inner = Self::generate_expr(&expr.then_branch.expr);
                let mut else_inner = Self::generate_expr(&expr.else_branch.1.expr);
                let mut else_ifs = String::new();
                for (_, _, cond, block) in &expr.elseif_branches {
                    let mut condition = Self::generate_expr(&cond);
                    let mut if_inner = Self::generate_expr(&block.expr);
                    else_ifs.push_str(&format!("else if ({}) {{return {};}}", condition, if_inner));
                }
                inner.push_str(&format!("if({}){{return {};}}{}else{{return {};}}", condition, if_inner, else_ifs, else_inner));
                str.push_str(&format!("(() => {{{}}})()", inner));
                str
            }

            fn generate_for(expr: &For) -> String {
                let mut str = String::new();
                let binding = Self::generate_var(&expr.binding);
                let arr = Self::generate_expr(&expr.expr);
                let mut inner = Self::generate_expr(&expr.block.expr);
                str.push_str(&format!("({}).map(({}) => {{return {};}})", arr, binding, inner));
                str
            }
            fn generate_un_ap(expr: &UnaryAp) -> String {
                let e = Self::generate_expr(&expr.right);
                let op = Self::generate_un_op(&expr.op);
                format!("{}({})", op, e)
            }
            fn generate_lit(expr: &Lit) -> String {
                match expr {
                    Lit::Str(str) => str.lit.span.value().to_string(),
                    Lit::Int(i) => i.lit.span.value().to_string(),
                    Lit::Float(f) => f.lit.span.value().to_string(),
                    Lit::Bool(b) => b.text.span.value().to_string(),
                }
            }
            fn generate_var(expr: &Var) -> String {
                let prefix = if expr.scope == Scope::Global { "props." } else { "" };
                format!("{}{}", prefix, expr.name.text.span.value())
            }
            fn generate_group_expr(expr: &Group<Expr>) -> String {
                let inner = Self::generate_expr(&expr.expr);
                format!("({})", inner)
            }
            fn generate_bin_ap(expr: &BinaryAp) -> String {
                let left = Self::generate_expr(&expr.left);
                let op = Self::generate_bin_op(&expr.right.op);
                let right = Self::generate_expr(&expr.right.right);
                format!("{} {} {}", left, op, right)
            }
            fn generate_sel_ap(expr: &SelectorAp) -> String {
                let left = Self::generate_expr(&expr.expr);
                let right = match &expr.right.selector {
                    SelectorOp::Named(named) => format!(".{}", named.name.text.span.value()),
                    SelectorOp::Bracket(bracket) => {
                        let inner = Self::generate_expr(&bracket.expr);
                        format!("[{}]", inner)
                    }
                };
                format!("({}){}", left, right)
            }
            fn generate_ap(expr: &Ap) -> String {
                let left = Self::generate_expr(&expr.expr);
                let args = Self::generate_punctuated_expr(&expr.right.group.expr);
                format!("({})({})", left, args)
            }
            fn generate_empty(empty: &Empty) -> String  {
                "undefined".to_string()
            }

            fn generate_expr(expr: &Expr) -> String {
                match expr {
                    Expr::If(if0) => Self::generate_if(if0),
                    Expr::For(for0) => Self::generate_for(for0),
                    Expr::UnaryAp(un_ap) => Self::generate_un_ap(un_ap),
                    Expr::Lit(lit) => Self::generate_lit(lit),
                    Expr::Var(var) => Self::generate_var(var),
                    Expr::Node(node) => Self::generate_node(node),
                    Expr::Empty(empty) => Self::generate_empty(empty),
                    Expr::Group(group) => Self::generate_group_expr(group),
                    Expr::BinaryAp(bin_ap) => Self::generate_bin_ap(bin_ap),
                    Expr::SelectorAp(sel_ap) => Self::generate_sel_ap(sel_ap),
                    Expr::Ap(ap) => Self::generate_ap(ap)
                }
            }

            fn generate_attribute_value(val: &AttributeValue) -> String {
                let mut str = String::new();
                match val {
                    AttributeValue::StrLit(lit) => {
                        str.push_str(lit.lit.span.value());
                    }
                    AttributeValue::Block(block) => {
                        let inner = Self::generate_expr(&block.expr);
                        str.push_str(&format!("(()=>{{{}}})()", inner));
                    }
                }
                str
            }

            fn generate_attribute(attr: &Attribute) -> String {
                let value = match &attr.value {
                    AttributeValue::StrLit(lit) => lit.lit.span.value().to_string(),
                    AttributeValue::Block(block) => Self::generate_expr(&block.expr)
                };
                format!("{}:{}", attr.name.text.span.value(), value)
            }

            fn generate_attributes(attrs: &Vec<Attribute>) -> String {
                let inner = attrs.iter()
                    .map(|attr| Self::generate_attribute(attr))
                    .collect::<Vec<String>>()
                    .join(",");
                format!("{{{}}}", inner)
            }

            fn generate_tag(tag: &TagNode) -> String {
                let tag_name = tag.name.text.span.value();
                let attrs = Self::generate_attributes(&tag.attributes);
                match &tag.block {
                    Some(tag_block) => {
                        let mut children = String::new();
                        let mut i = 0;
                        for child in &tag_block.children {
                            let e = Self::generate_node_or_block(&child);
                            if i != 0 {
                                children.push_str(",");
                            }
                            children.push_str(&e);
                            i += 1;
                        }
                        format!("config.createElement(`{}`, {}, [{}])", tag_name, attrs, children)
                    },
                    None => {
                        format!("config.createElement(`{}`, {})", tag_name, attrs)
                    }
                }
            }

            pub fn generate_node(nob: &Node) -> String {

                match nob {
                    Node::Text(text) => {
                        let mut str = String::new();
                        str.push_str(&format!("config.createTextNode(`{}`)", Self::escape(text.text.span.value())));
                        str
                    },
                    Node::Tag(tag) => Self::generate_tag(tag)
                }
            }

            pub fn generate_block(nob: &Block) -> String {
                Self::generate_expr(&nob.expr)
            }


            pub fn generate_node_or_block(nob: &NodeOrBlock) -> String {
                match nob {
                    NodeOrBlock::Node(node) => Self::generate_node(node),
                    NodeOrBlock::Block(block) => Self::generate_block(block)
                }
            }

            fn generate_type(typ: Type) -> String {
                match typ {
                    Type::Any => "any",
                    Type::Function => "(..._:any[]) => any",
                    Type::Unit => "void",
                    Type::Array => "any[]",
                    Type::Object => "{}",
                    Type::String => "string",
                    Type::Int => "number",
                    Type::Float => "number",
                    Type::Bool => "boolean",
                    Type::HtmlElement => "HTMLElement"
                }.to_string()
            }

            fn generate_globals_type(globals: &HashSet<Var>) -> String {
                let mut str = String::new();
                str.push_str("{");
                for var in globals {
                    str.push_str(&format!("{}: {}", var.name.text.span.value(), Self::generate_type(var.typ)));
                }
                str.push_str("}");
                str
            }

            pub fn generate(&self, view: &View) -> String {
                let mut str = String::new();
                let globals = view.globals();
                //let globals_type = Self::generate_globals_type(&globals);
                str.push_str(&format!("module.exports = function(props, config) {{return "));
                match &view.root {
                    None => {},
                    Some(nob) =>  str.push_str(&Self::generate_node_or_block(&nob))
                }
                str.push_str(";}");
                str
            }
        }
    }
}

pub mod rs {

}