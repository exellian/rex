pub mod js {

    pub struct JsCodegen;

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
                JsCodegen
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

            fn generate_punctuated_expr(mut punct: &Punctuated<Expr, Comma>, in_attr: bool) -> String {
                let mut str = String::new();
                let e = Self::generate_expr(&punct.expr, in_attr);
                str.push_str(&e);
                let mut other = &punct.other;
                while let Some((_, o)) = other {
                    let e = Self::generate_expr(&o.expr, in_attr);
                    str.push_str(",");
                    str.push_str(&e);
                    other = &o.other
                }
                str
            }

            fn generate_if(expr: &If, in_attr: bool) -> String {
                let mut str = String::new();
                let mut inner = String::new();
                let mut condition = Self::generate_expr(&expr.condition, in_attr);
                let mut if_inner = Self::generate_expr(&expr.then_branch.expr, in_attr);
                let mut else_inner = Self::generate_expr(&expr.else_branch.1.expr, in_attr);
                let mut else_ifs = String::new();
                for (_, _, cond, block) in &expr.elseif_branches {
                    let mut condition = Self::generate_expr(&cond, in_attr);
                    let mut if_inner = Self::generate_expr(&block.expr, in_attr);
                    else_ifs.push_str(&format!("else if ({}) {{return {};}}", condition, if_inner));
                }
                inner.push_str(&format!("if({}){{return {};}}{}else{{return {};}}", condition, if_inner, else_ifs, else_inner));
                str.push_str(&format!("(() => {{{}}})()", inner));
                str
            }

            fn generate_for(expr: &For, in_attr: bool) -> String {
                let mut str = String::new();
                let binding = Self::generate_var(&expr.binding, in_attr);
                let arr = Self::generate_expr(&expr.expr, in_attr);
                let mut inner = Self::generate_expr(&expr.block.expr, in_attr);
                str.push_str(&format!("({}).map(({}) => {{return {};}})", arr, binding, inner));
                str
            }
            fn generate_un_ap(expr: &UnaryAp, in_attr: bool) -> String {
                let e = Self::generate_expr(&expr.right, in_attr);
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
            fn generate_var(expr: &Var, in_attr: bool) -> String {
                let prefix = if expr.scope == Scope::Global { "props." } else { "" };
                format!("{}{}", prefix, expr.name.text.span.value())
            }
            fn generate_group_expr(expr: &Group<Expr>, in_attr: bool) -> String {
                let inner = Self::generate_expr(&expr.expr, in_attr);
                format!("({})", inner)
            }
            fn generate_bin_ap(expr: &BinaryAp, in_attr: bool) -> String {
                let left = Self::generate_expr(&expr.left, in_attr);
                let op = Self::generate_bin_op(&expr.right.op);
                let right = Self::generate_expr(&expr.right.right, in_attr);
                format!("{} {} {}", left, op, right)
            }
            fn generate_sel_ap(expr: &SelectorAp, in_attr: bool) -> String {
                let left = Self::generate_expr(&expr.expr, in_attr);
                let right = match &expr.right.selector {
                    SelectorOp::Named(named) => format!(".{}", named.name.text.span.value()),
                    SelectorOp::Bracket(bracket) => {
                        let inner = Self::generate_expr(&bracket.expr, in_attr);
                        format!("[{}]", inner)
                    }
                };
                format!("({}){}", left, right)
            }
            fn generate_ap(expr: &Ap, in_attr: bool) -> String {
                let left = Self::generate_expr(&expr.expr, in_attr);
                let args = Self::generate_punctuated_expr(&expr.right.group.expr, in_attr);
                format!("({})({})", left, args)
            }
            fn generate_empty(empty: &Empty) -> String  {
                "undefined".to_string()
            }

            fn generate_expr(expr: &Expr, in_attr: bool) -> String {
                match expr {
                    Expr::If(if0) => Self::generate_if(if0, in_attr),
                    Expr::For(for0) => Self::generate_for(for0, in_attr),
                    Expr::UnaryAp(un_ap) => Self::generate_un_ap(un_ap, in_attr),
                    Expr::Lit(lit) => Self::generate_lit(lit),
                    Expr::Var(var) => Self::generate_var(var, in_attr),
                    Expr::Node(node) => Self::generate_node(node, in_attr),
                    Expr::Empty(empty) => Self::generate_empty(empty),
                    Expr::Group(group) => Self::generate_group_expr(group, in_attr),
                    Expr::BinaryAp(bin_ap) => Self::generate_bin_ap(bin_ap, in_attr),
                    Expr::SelectorAp(sel_ap) => Self::generate_sel_ap(sel_ap, in_attr),
                    Expr::Ap(ap) => Self::generate_ap(ap, in_attr)
                }
            }


            fn generate_attribute(attr: &Attribute) -> String {
                let value = match &attr.value {
                    AttributeValue::StrLit(lit) => lit.lit.span.value().to_string(),
                    AttributeValue::Block(block) => Self::generate_expr(&block.expr, true)
                };
                format!("{}: () => {}", attr.name.text.span.value(), value)
            }

            fn generate_attributes(attrs: &Vec<Attribute>) -> String {
                let inner = attrs.iter()
                    .map(|attr| Self::generate_attribute(attr))
                    .collect::<Vec<String>>()
                    .join(",");
                format!("{{{}}}", inner)
            }

            fn generate_tag(tag: &TagNode, in_attr: bool) -> String {
                let tag_name = tag.name.text.span.value();
                let attrs = Self::generate_attributes(&tag.attributes);
                let el_name = if in_attr { "el" } else { "domEl" };
                match &tag.block {
                    Some(tag_block) => {
                        let mut children = String::new();
                        let mut i = 0;
                        for child in &tag_block.children {
                            let e = Self::generate_node_or_block(&child, in_attr);
                            if i != 0 {
                                children.push_str(",");
                            }

                            children.push_str(&format!("() => {}", e));
                            i += 1;
                        }
                        format!("config.{}(`{}`, {}, [{}])", el_name, tag_name, attrs, children)
                    },
                    None => {
                        format!("config.{}(`{}`, {})", el_name, tag_name, attrs)
                    }
                }
            }

            pub fn generate_node(nob: &Node, in_attr: bool) -> String {
                let text_name = if in_attr { "text" } else { "domText" };
                match nob {
                    Node::Text(text) => {
                        let mut str = String::new();
                        str.push_str(&format!("config.{}(`{}`)", text_name, Self::escape(text.text.span.value())));
                        str
                    },
                    Node::Tag(tag) => Self::generate_tag(tag, in_attr)
                }
            }

            pub fn generate_block(nob: &Block, in_attr: bool) -> String {
                Self::generate_expr(&nob.expr, in_attr)
            }


            pub fn generate_node_or_block(nob: &NodeOrBlock, in_attr: bool) -> String {
                match nob {
                    NodeOrBlock::Node(node) => Self::generate_node(node, in_attr),
                    NodeOrBlock::Block(block) => Self::generate_block(block, in_attr)
                }
            }

            pub fn generate(&self, view: &View) -> String {
                let mut str = String::new();
                str.push_str(&format!("module.exports = {{render: function(props, config) {{return "));
                match &view.root {
                    None => {},
                    Some(nob) =>  str.push_str(&Self::generate_node_or_block(&nob, false))
                }
                str.push_str(";}}");
                str
            }
        }
    }
}

pub mod rs {

    pub struct RsCodegen;

    mod implementation {
        use std::collections::HashSet;
        use crate::codegen::rs::RsCodegen;
        use crate::rex::parse::{Ap, Attribute, AttributeValue, BinaryAp, BinaryOp, Block, Expr, For, Group, If, Node, NodeOrBlock, Punctuated, SelectorAp, SelectorOp, TagNode, UnaryAp, UnaryOp, Var};
        use crate::rex::parse::primitive::{Comma, Empty, Lit};
        use crate::rex::parse::scope::Scope;
        use crate::rex::parse::typ::{AbstractType, PrimitiveType, Type};
        use crate::rex::View;

        struct Generics {
            generics: Vec<(String, Option<String>)>,
            count: usize
        }

        impl Generics {

            pub fn new() -> Self {
                Generics { generics: vec![], count: 0 }
            }

            pub fn next(&mut self, bounds: Option<&str>) -> String {
                let res = if self.count == 0 {
                    "T".into()
                } else {
                    format!("T{}", self.count)
                };
                self.count += 1;
                self.generics.push((res.clone(), bounds.map(|x| x.into())));
                res
            }
        }

        impl RsCodegen {
            pub fn new() -> Self {
                RsCodegen
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

            fn generate_punctuated_expr(mut punct: &Punctuated<Expr, Comma>, in_attr: bool) -> String {
                let mut str = String::new();
                let e = Self::generate_expr(&punct.expr, in_attr);
                str.push_str(&e);
                let mut other = &punct.other;
                while let Some((_, o)) = other {
                    let e = Self::generate_expr(&o.expr, in_attr);
                    str.push_str(",");
                    str.push_str(&e);
                    other = &o.other
                }
                str
            }

            fn generate_if(expr: &If, in_attr: bool) -> String {
                let mut str = String::new();
                let mut inner = String::new();
                let mut condition = Self::generate_expr(&expr.condition, in_attr);
                let mut if_inner = Self::generate_expr(&expr.then_branch.expr, in_attr);
                let mut else_inner = Self::generate_expr(&expr.else_branch.1.expr, in_attr);
                let mut else_ifs = String::new();
                for (_, _, cond, block) in &expr.elseif_branches {
                    let mut condition = Self::generate_expr(&cond, in_attr);
                    let mut if_inner = Self::generate_expr(&block.expr, in_attr);
                    else_ifs.push_str(&format!("else if ({}) {{return {};}}", condition, if_inner));
                }
                inner.push_str(&format!("if({}){{return {};}}{}else{{return {};}}", condition, if_inner, else_ifs, else_inner));
                str.push_str(&format!("(() => {{{}}})()", inner));
                str
            }

            fn generate_for(expr: &For, in_attr: bool) -> String {
                let mut str = String::new();
                let binding = Self::generate_var(&expr.binding, in_attr);
                let arr = Self::generate_expr(&expr.expr, in_attr);
                let mut inner = Self::generate_expr(&expr.block.expr, in_attr);
                str.push_str(&format!("({}).map(({}) => {{return {};}})", arr, binding, inner));
                str
            }
            fn generate_un_ap(expr: &UnaryAp, in_attr: bool) -> String {
                let e = Self::generate_expr(&expr.right, in_attr);
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
            fn generate_var(expr: &Var, in_attr: bool) -> String {
                let prefix = if expr.scope == Scope::Global { "props." } else { "" };
                format!("{}{}", prefix, expr.name.text.span.value())
            }
            fn generate_group_expr(expr: &Group<Expr>, in_attr: bool) -> String {
                let inner = Self::generate_expr(&expr.expr, in_attr);
                format!("({})", inner)
            }
            fn generate_bin_ap(expr: &BinaryAp, in_attr: bool) -> String {
                let left = Self::generate_expr(&expr.left, in_attr);
                let op = Self::generate_bin_op(&expr.right.op);
                let right = Self::generate_expr(&expr.right.right, in_attr);
                format!("{} {} {}", left, op, right)
            }
            fn generate_sel_ap(expr: &SelectorAp, in_attr: bool) -> String {
                let left = Self::generate_expr(&expr.expr, in_attr);
                let right = match &expr.right.selector {
                    SelectorOp::Named(named) => format!(".{}", named.name.text.span.value()),
                    SelectorOp::Bracket(bracket) => {
                        let inner = Self::generate_expr(&bracket.expr, in_attr);
                        format!("[{}]", inner)
                    }
                };
                format!("({}){}", left, right)
            }
            fn generate_ap(expr: &Ap, in_attr: bool) -> String {
                let left = Self::generate_expr(&expr.expr, in_attr);
                let args = Self::generate_punctuated_expr(&expr.right.group.expr, in_attr);
                format!("({})({})", left, args)
            }
            fn generate_empty(empty: &Empty) -> String  {
                "undefined".to_string()
            }

            fn generate_expr(expr: &Expr, in_attr: bool) -> String {
                match expr {
                    Expr::If(if0) => Self::generate_if(if0, in_attr),
                    Expr::For(for0) => Self::generate_for(for0, in_attr),
                    Expr::UnaryAp(un_ap) => Self::generate_un_ap(un_ap, in_attr),
                    Expr::Lit(lit) => Self::generate_lit(lit),
                    Expr::Var(var) => Self::generate_var(var, in_attr),
                    Expr::Node(node) => Self::generate_node(node, in_attr),
                    Expr::Empty(empty) => Self::generate_empty(empty),
                    Expr::Group(group) => Self::generate_group_expr(group, in_attr),
                    Expr::BinaryAp(bin_ap) => Self::generate_bin_ap(bin_ap, in_attr),
                    Expr::SelectorAp(sel_ap) => Self::generate_sel_ap(sel_ap, in_attr),
                    Expr::Ap(ap) => Self::generate_ap(ap, in_attr)
                }
            }


            fn generate_attribute(attr: &Attribute) -> String {
                let value = match &attr.value {
                    AttributeValue::StrLit(lit) => lit.lit.span.value().to_string(),
                    AttributeValue::Block(block) => Self::generate_expr(&block.expr, true)
                };
                format!("{}: () => {}", attr.name.text.span.value(), value)
            }

            fn generate_attributes(attrs: &Vec<Attribute>) -> String {
                let inner = attrs.iter()
                    .map(|attr| Self::generate_attribute(attr))
                    .collect::<Vec<String>>()
                    .join(",");
                format!("{{{}}}", inner)
            }

            fn generate_tag(tag: &TagNode, in_attr: bool) -> String {
                let tag_name = tag.name.text.span.value();
                let attrs = Self::generate_attributes(&tag.attributes);
                let el_name = if in_attr { "el" } else { "domEl" };
                match &tag.block {
                    Some(tag_block) => {
                        let mut children = String::new();
                        let mut i = 0;
                        for child in &tag_block.children {
                            let e = Self::generate_node_or_block(&child, in_attr);
                            if i != 0 {
                                children.push_str(",");
                            }

                            children.push_str(&format!("() => {}", e));
                            i += 1;
                        }
                        format!("config.{}(`{}`, {}, [{}])", el_name, tag_name, attrs, children)
                    },
                    None => {
                        format!("config.{}(`{}`, {})", el_name, tag_name, attrs)
                    }
                }
            }

            pub fn generate_node(nob: &Node, in_attr: bool) -> String {
                let text_name = if in_attr { "text" } else { "domText" };
                match nob {
                    Node::Text(text) => {
                        let mut str = String::new();
                        str.push_str(&format!("config.{}(`{}`)", text_name, Self::escape(text.text.span.value())));
                        str
                    },
                    Node::Tag(tag) => Self::generate_tag(tag, in_attr)
                }
            }

            pub fn generate_block(nob: &Block, in_attr: bool) -> String {
                Self::generate_expr(&nob.expr, in_attr)
            }


            pub fn generate_node_or_block(nob: &NodeOrBlock, in_attr: bool) -> String {
                match nob {
                    NodeOrBlock::Node(node) => Self::generate_node(node, in_attr),
                    NodeOrBlock::Block(block) => Self::generate_block(block, in_attr)
                }
            }

            fn generate_type(generics: &mut Generics, typ: &Type) -> String {

                match typ {
                    Type::Abstract(abs) => match abs {
                        AbstractType::Any => generics.next(Some("ToString")),
                        AbstractType::Number => "f32".into(),
                        AbstractType::IntLike => "usize".into(),
                        AbstractType::AddAndEq => generics.next(Some("std::ops::Add + std::ops::Eq"))
                    }
                    Type::Primitive(prim) => match prim {
                        PrimitiveType::Function(arg_types) => {
                            let args: Vec<String> = arg_types.iter().map(|typ| Self::generate_type(generics, typ)).collect();
                            let arg_list = args[0..args.len() - 1].join(",");
                            generics.next(Some(&format!("Fn({}) -> {}", arg_list, args[args.len() - 1])))
                        }
                        PrimitiveType::Unit => "()".into(),
                        PrimitiveType::Array(inner) => {
                            let inner = Self::generate_type(generics, inner);
                            format!("Vec<{}>", inner)
                        }
                        PrimitiveType::Object(_) => "".into(),
                        PrimitiveType::String => "String".into(),
                        PrimitiveType::Int => "i32".into(),
                        PrimitiveType::Float => "f32".into(),
                        PrimitiveType::Bool => "bool".into(),
                        PrimitiveType::Node => "NODE".into()
                    }
                }
            }

            fn generate_props_struct(globals: &HashSet<Var>) -> String {
                let mut generics = Generics::new();
                let mut str = String::new();
                str.push_str("struct Props {");
                for var in globals {
                    str.push_str(&format!("{}: {}", var.name.text.span.value(), Self::generate_type(&mut generics, &var.typ)));
                }
                str.push_str("}");
                str
            }

            pub fn generate(&self, view: &View) -> String {
                let mut str = String::new();
                let globals = view.globals();
                let props_struct = Self::generate_props_struct(&globals);
                str.push_str(&format!("{} fn render(props: &Props, config) {{return ", props_struct));
                match &view.root {
                    None => {},
                    Some(nob) =>  str.push_str(&Self::generate_node_or_block(&nob, false))
                }
                str.push_str(";}");
                str
            }
        }
    }
}