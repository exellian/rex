pub mod js {

    pub struct JsCodegen;

    mod implementation {
        use crate::codegen::js::JsCodegen;
        use crate::rex::parse::primitive::{Comma, Empty, Lit};
        use crate::rex::parse::scope::Scope;
        use crate::rex::parse::typ::Type;
        use crate::rex::parse::{
            Ap, Attribute, AttributeValue, BinaryAp, BinaryOp, Block, Expr, For, Group, If, Node,
            NodeOrBlock, Punctuated, SelectorAp, SelectorOp, TagNode, UnaryAp, UnaryOp, Var,
        };
        use crate::View;
        use std::collections::HashSet;

        impl JsCodegen {
            pub fn new() -> Self {
                JsCodegen
            }

            pub fn escape(str: &str) -> String {
                str.to_string()
            }

            fn generate_un_op(un_op: &UnaryOp) -> String {
                match un_op {
                    UnaryOp::Neg(_) => "-".to_string(),
                    UnaryOp::Not(_) => "!".to_string(),
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
                    BinaryOp::Gt(_) => ">".to_string(),
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
                inner.push_str(&format!(
                    "if({}){{return {};}}{}else{{return {};}}",
                    condition, if_inner, else_ifs, else_inner
                ));
                str.push_str(&format!("(() => {{{}}})()", inner));
                str
            }

            fn generate_for(expr: &For) -> String {
                let mut str = String::new();
                let binding = Self::generate_var(&expr.binding);
                let arr = Self::generate_expr(&expr.expr);
                let inner = Self::generate_expr(&expr.block.expr);
                str.push_str(&format!(
                    "({}).map(({}) => {{return {};}})",
                    arr, binding, inner
                ));
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
                let prefix = if expr.scope == Scope::Global {
                    "props."
                } else {
                    ""
                };
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
            fn generate_empty(empty: &Empty) -> String {
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
                    Expr::Ap(ap) => Self::generate_ap(ap),
                }
            }

            fn generate_attribute(attr: &Attribute) -> String {
                let value = match &attr.value {
                    AttributeValue::StrLit(lit) => lit.lit.span.value().to_string(),
                    AttributeValue::Block(block) => Self::generate_expr(&block.expr),
                };
                format!("{}: () => {}", attr.name.text.span.value(), value)
            }

            fn generate_attributes(attrs: &Vec<Attribute>) -> String {
                let inner = attrs
                    .iter()
                    .map(|attr| Self::generate_attribute(attr))
                    .collect::<Vec<String>>()
                    .join(",");
                format!("{{{}}}", inner)
            }

            fn generate_tag(tag: &TagNode) -> String {
                let tag_name = tag.name.text.span.value();
                let attrs = Self::generate_attributes(&tag.attributes);
                let el_name = "el";
                match &tag.block {
                    Some(tag_block) => {
                        let mut children = String::new();
                        let mut i = 0;
                        for child in &tag_block.children {
                            let e = Self::generate_node_or_block(&child);
                            if i != 0 {
                                children.push_str(",");
                            }

                            children.push_str(&format!("() => {}", e));
                            i += 1;
                        }
                        format!(
                            "config.{}(`{}`, {}, [{}])",
                            el_name, tag_name, attrs, children
                        )
                    }
                    None => {
                        format!("config.{}(`{}`, {})", el_name, tag_name, attrs)
                    }
                }
            }

            pub fn generate_node(nob: &Node) -> String {
                let text_name = "text";
                match nob {
                    Node::Text(text) => {
                        let mut str = String::new();
                        str.push_str(&format!(
                            "config.{}(`{}`)",
                            text_name,
                            Self::escape(text.text.span.value())
                        ));
                        str
                    }
                    Node::Tag(tag) => Self::generate_tag(tag),
                }
            }

            pub fn generate_block(nob: &Block) -> String {
                Self::generate_expr(&nob.expr)
            }

            pub fn generate_node_or_block(nob: &NodeOrBlock) -> String {
                match nob {
                    NodeOrBlock::Node(node) => Self::generate_node(node),
                    NodeOrBlock::Block(block) => Self::generate_block(block),
                }
            }

            pub fn generate(&self, view: &View) -> String {
                let mut str = String::new();
                str.push_str(&format!(
                    "module.exports = {{render: function(props, config) {{return "
                ));
                match &view.root {
                    None => {}
                    Some(nob) => str.push_str(&Self::generate_node_or_block(&nob)),
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
        use crate::codegen::rs::RsCodegen;
        use crate::rex::parse::primitive::{Comma, Empty, Lit};
        use crate::rex::parse::scope::Scope;
        use crate::rex::parse::typ::{AbstractType, PrimitiveType, Type};
        use crate::rex::parse::{
            Ap, Attribute, AttributeValue, BinaryAp, BinaryOp, Block, Expr, For, Group, If, Node,
            NodeOrBlock, Punctuated, SelectorAp, SelectorOp, TagNode, TextNode, UnaryAp, UnaryOp,
            Var,
        };
        use crate::rex::View;
        use crate::util::HashMultimap;
        use std::collections::{BTreeMap, HashMap};

        struct Structs {
            structs: HashMap<BTreeMap<String, Type>, String>,
            name_counts: HashMap<String, usize>,
            anonymous_count: usize,
        }

        impl Structs {
            pub fn new() -> Self {
                Structs {
                    structs: HashMap::new(),
                    name_counts: HashMap::new(),
                    anonymous_count: 0,
                }
            }

            fn get_anonymous_name(&mut self) -> String {
                let res = format!("A{}", self.anonymous_count);
                self.anonymous_count += 1;
                res
            }

            pub fn get_name(
                &mut self,
                mapping: &BTreeMap<String, Type>,
                prop_name_opt: Option<String>,
            ) -> String {
                match self.structs.get(mapping) {
                    None => {}
                    Some(name) => return name.clone(),
                }
                let prop_name = if let Some(n) = prop_name_opt {
                    n
                } else {
                    self.get_anonymous_name()
                };
                let mut prop_name_iter = prop_name.chars();
                let first = prop_name_iter.next().unwrap().to_uppercase();
                let rest: String = prop_name_iter.collect();
                let prefix = format!("{}{}", first, rest);
                let postfix = match self.name_counts.get_mut(&prefix) {
                    None => {
                        self.name_counts.insert(prefix.clone(), 1);
                        "".to_string()
                    }
                    Some(count) => {
                        *count += 1;
                        format!("{}", count)
                    }
                };
                let name = format!("{}{}", prefix, postfix);
                self.structs.insert(mapping.clone(), name.clone());
                name
            }
        }

        struct Generics {
            generics: Vec<(String, Option<String>)>,
            count: usize,
        }

        impl Generics {
            pub fn new() -> Self {
                Generics {
                    generics: vec![],
                    count: 0,
                }
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

            pub fn type_var_str(&self) -> String {
                self.generics
                    .iter()
                    .map(|(ty_ov_name, _)| ty_ov_name.clone())
                    .collect::<Vec<String>>()
                    .join(",")
            }

            pub fn type_var_def_str(&self) -> String {
                self.generics
                    .iter()
                    .map(|(ty_ov_name, bound_opt)| {
                        if let Some(bound) = bound_opt {
                            format!("{}: {}", ty_ov_name, bound)
                        } else {
                            format!("{}", ty_ov_name)
                        }
                    })
                    .collect::<Vec<String>>()
                    .join(",")
            }
        }

        impl RsCodegen {
            pub fn new() -> Self {
                RsCodegen
            }

            pub fn escape(str: &str) -> String {
                str.to_string()
            }

            fn wrap_ref(code: String, as_ref: bool) -> String {
                let ref_prefix = if as_ref { "&" } else { "" };
                format!("{}{}", ref_prefix, code)
            }

            fn flattenable_typ(typ: &Type) -> Option<PrimitiveType> {
                if let Type::Primitive(PrimitiveType::Array(inner_typ)) = typ {
                    return Self::flattenable_typ(&*inner_typ);
                }
                if let Type::Primitive(inner) = typ {
                    return Some(inner.clone());
                }
                return None;
            }

            fn generate_un_op(un_op: &UnaryOp) -> String {
                match un_op {
                    UnaryOp::Neg(_) => "-".to_string(),
                    UnaryOp::Not(_) => "!".to_string(),
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
                    BinaryOp::Gt(_) => ">".to_string(),
                }
            }

            fn generate_punctuated_expr(punct: &Punctuated<Expr, Comma>, in_attr: bool) -> String {
                let mut str = String::new();
                let e = Self::generate_expr(&punct.expr, false, in_attr);
                str.push_str(&e);
                let mut other = &punct.other;
                while let Some((_, o)) = other {
                    let e = Self::generate_expr(&o.expr, false, in_attr);
                    str.push_str(",");
                    str.push_str(&e);
                    other = &o.other
                }
                str
            }

            fn generate_if(expr: &If, as_ref: bool, in_attr: bool) -> String {
                let mut str = String::new();
                let condition = Self::generate_expr(&expr.condition, false, in_attr);
                let if_inner = Self::generate_expr(&expr.then_branch.expr, as_ref, in_attr);
                let else_inner = Self::generate_expr(&expr.else_branch.1.expr, as_ref, in_attr);
                let mut else_ifs = String::new();
                for (_, _, cond, block) in &expr.elseif_branches {
                    let condition = Self::generate_expr(&cond, false, in_attr);
                    let if_inner = Self::generate_expr(&block.expr, as_ref, in_attr);
                    else_ifs.push_str(&format!("else if {} {{{}}}", condition, if_inner));
                }
                str.push_str(&format!(
                    "if{} {{{}}}{}else{{{}}}",
                    condition, if_inner, else_ifs, else_inner
                ));
                str
            }

            fn generate_for(expr: &For, in_attr: bool) -> String {
                let binding = Self::generate_var(&expr.binding, false);
                let arr = Self::generate_expr(&expr.expr, false, in_attr);
                let inner = Self::generate_expr(&expr.block.expr, true, in_attr);
                assert!(expr.expr.typ().is_array());
                format!(
                    "({}).iter().map(|{}| {{{}}}).collect::<Vec<_>>()",
                    arr, binding, inner
                )
            }

            fn generate_un_ap(expr: &UnaryAp, in_attr: bool) -> String {
                let e = Self::generate_expr(&expr.right, false, in_attr);
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
            fn generate_var(expr: &Var, as_ref: bool) -> String {
                let prefix = if expr.scope == Scope::Global {
                    "props."
                } else {
                    ""
                };

                let res = format!("{}{}", prefix, expr.name.text.span.value());
                Self::wrap_ref(res, as_ref)
            }

            fn generate_group_expr(expr: &Group<Expr>, as_ref: bool, in_attr: bool) -> String {
                let inner = Self::generate_expr(&expr.expr, as_ref, in_attr);
                format!("({})", inner)
            }

            fn generate_bin_ap(expr: &BinaryAp, as_ref: bool, in_attr: bool) -> String {
                let left = Self::generate_expr(&expr.left, false, in_attr);
                let op = Self::generate_bin_op(&expr.right.op);
                let right = Self::generate_expr(&expr.right.right, false, in_attr);
                Self::wrap_ref(format!("({} {} {})", left, op, right), as_ref)
            }

            fn generate_sel_ap(expr: &SelectorAp, as_ref: bool, in_attr: bool) -> String {
                let left = Self::generate_expr(&expr.expr, false, in_attr);
                let right = match &expr.right.selector {
                    SelectorOp::Named(named) => format!(".{}", named.name.text.span.value()),
                    SelectorOp::Bracket(bracket) => {
                        let inner = Self::generate_expr(&bracket.expr, false, in_attr);
                        format!("{}[0]", inner)
                    }
                };
                Self::wrap_ref(format!("({}){}", left, right), as_ref)
            }
            fn generate_ap(expr: &Ap, as_ref: bool, in_attr: bool) -> String {
                let left = Self::generate_expr(&expr.expr, false, in_attr);
                let args = Self::generate_punctuated_expr(&expr.right.group.expr, in_attr);
                Self::wrap_ref(format!("({})({})", left, args), as_ref)
            }
            fn generate_empty(_: &Empty) -> String {
                "()".to_string()
            }

            fn generate_expr(expr: &Expr, as_ref: bool, in_attr: bool) -> String {
                match expr {
                    Expr::If(if0) => Self::generate_if(if0, as_ref, in_attr),
                    Expr::For(for0) => Self::generate_for(for0, in_attr),
                    Expr::UnaryAp(un_ap) => Self::generate_un_ap(un_ap, in_attr),
                    Expr::Lit(lit) => Self::generate_lit(lit),
                    Expr::Var(var) => Self::generate_var(var, as_ref),
                    Expr::Node(node) => Self::generate_node(node, in_attr),
                    Expr::Empty(empty) => Self::generate_empty(empty),
                    Expr::Group(group) => Self::generate_group_expr(group, as_ref, in_attr),
                    Expr::BinaryAp(bin_ap) => Self::generate_bin_ap(bin_ap, as_ref, in_attr),
                    Expr::SelectorAp(sel_ap) => Self::generate_sel_ap(sel_ap, as_ref, in_attr),
                    Expr::Ap(ap) => Self::generate_ap(ap, as_ref, in_attr),
                }
            }

            fn generate_attribute(attr: &Attribute) -> String {
                let value = match &attr.value {
                    AttributeValue::StrLit(lit) => lit.lit.span.value().to_string(),
                    AttributeValue::Block(block) => Self::generate_expr(&block.expr, false, true),
                };
                format!(
                    "(\"{}\".to_string(), C::attr({}))",
                    attr.name.text.span.value(),
                    value
                )
            }

            fn generate_attributes(attrs: &Vec<Attribute>) -> String {
                let inner = attrs
                    .iter()
                    .map(|attr| Self::generate_attribute(attr))
                    .collect::<Vec<String>>()
                    .join(",");
                format!("HashMap::from([{}])", inner)
            }

            fn generate_tag(tag: &TagNode, in_attr: bool) -> String {
                let tag_name = tag.name.text.span.value();
                let attrs = Self::generate_attributes(&tag.attributes);
                let node = match &tag.block {
                    Some(tag_block) => {
                        let mut children = vec![];
                        let mut i = 0;
                        for child in &tag_block.children {
                            let typ = match child {
                                NodeOrBlock::Node(_) => &Type::NODE,
                                NodeOrBlock::Block(b) => b.expr.typ(),
                            };
                            let mut e = Self::generate_node_or_block(&child, true, in_attr);
                            let flattenable = Self::flattenable_typ(typ);
                            if let Some(PrimitiveType::Node) = &flattenable {
                                e = format!("({}).flatten()", e);
                            } else {
                                e = format!("C::text({}, {})", e, in_attr);
                            }
                            children.push(format!("{}", e));
                            i += 1;
                        }
                        let flatten = if children.is_empty() {
                            ""
                        } else {
                            ".flatten()"
                        };
                        format!(
                            "C::el(\"{}\", || {}, || vec![{}]{}, {})",
                            tag_name,
                            attrs,
                            children.join(",\n"),
                            flatten,
                            in_attr
                        )
                    }
                    None => {
                        format!("C::el(\"{}\", {})", tag_name, attrs)
                    }
                };
                node
            }

            pub fn generate_text_node(text: &TextNode, in_attr: bool) -> String {
                format!(
                    "C::text(\"{}\", {})",
                    Self::escape(text.text.span.value()),
                    in_attr
                )
            }

            pub fn generate_node(node: &Node, in_attr: bool) -> String {
                match node {
                    Node::Text(text) => Self::generate_text_node(text, in_attr),
                    Node::Tag(tag) => Self::generate_tag(tag, in_attr),
                }
            }

            pub fn generate_block(nob: &Block, as_ref: bool, in_attr: bool) -> String {
                Self::generate_expr(&nob.expr, as_ref, in_attr)
            }

            pub fn generate_node_or_block(
                nob: &NodeOrBlock,
                as_ref: bool,
                in_attr: bool,
            ) -> String {
                match nob {
                    NodeOrBlock::Node(node) => Self::generate_node(node, in_attr),
                    NodeOrBlock::Block(block) => Self::generate_block(block, as_ref, in_attr),
                }
            }

            fn generate_type(generics: &mut Generics, typ: &Type) -> String {
                match typ {
                    Type::Abstract(abs) => match abs {
                        AbstractType::Any => generics.next(Some("Render")),
                        AbstractType::Number => "f32".into(),
                        AbstractType::IntLike => "usize".into(),
                        AbstractType::AddAndEq => {
                            generics.next(Some("std::ops::Add + std::ops::Eq + Render"))
                        }
                    },
                    Type::Primitive(prim) => match prim {
                        PrimitiveType::Function(arg_types) => {
                            let args: Vec<String> = arg_types
                                .iter()
                                .map(|typ| Self::generate_type(generics, typ))
                                .collect();
                            let arg_list = args[0..args.len() - 1].join(",");
                            generics.next(Some(&format!(
                                "Fn({}) -> {}",
                                arg_list,
                                args[args.len() - 1]
                            )))
                        }
                        PrimitiveType::Unit => "()".into(),
                        PrimitiveType::Array(inner) => {
                            let inner = Self::generate_type(generics, inner);
                            format!("Vec<{}>", inner)
                        }
                        PrimitiveType::Object(mapping) => {
                            let fields = mapping
                                .into_iter()
                                .map(|(name, typ)| {
                                    (name.clone(), Self::generate_type(generics, typ))
                                })
                                .map(|(name, typ_s)| format!("{}: {}", name, typ_s))
                                .collect::<Vec<String>>()
                                .join(",");
                            format!("Structx!{{ {} }}", fields)
                        }
                        PrimitiveType::String => "String".into(),
                        PrimitiveType::Int => "i32".into(),
                        PrimitiveType::Float => "f32".into(),
                        PrimitiveType::Bool => "bool".into(),
                        PrimitiveType::Node => "NODE".into(),
                    },
                }
            }

            pub fn generate_globals_mapping(
                globals: HashMultimap<String, &Var>,
            ) -> BTreeMap<String, Type> {
                globals
                    .into_iter()
                    .map(|(name, set)| (name, set.iter().next().unwrap().typ.clone()))
                    .collect()
            }

            pub fn generate(&self, view: &View) -> String {
                let mut str = String::new();
                let globals = Self::generate_globals_mapping(view.globals());
                println!("{:?}", globals);

                let mut generics = Generics::new();
                let props_type_str = Self::generate_type(
                    &mut generics,
                    &Type::Primitive(PrimitiveType::Object(globals)),
                );
                let generics_def_str = generics.type_var_def_str();
                let generics_str = generics.type_var_str();
                let props_type = format!("pub type Props<{}> = {};", &generics_str, props_type_str);
                str.push_str(&format!("use rex::{{Config, ChildFn, AttributeFn, ChildValue, AttributeValue}}; use structx::*; {} pub fn render<{}, C: Config>(props: &Props<{}>) -> Vec<C::Node> {{", props_type, &generics_def_str, &generics_str));
                match &view.root {
                    None => {}
                    Some(nob) => str.push_str(&Self::generate_node_or_block(&nob, false, false)),
                }
                str.push_str("}");
                str
            }
        }
    }
}
