use serde_json::{Map, Number, Value};
use std::collections::BTreeMap;
use std::rc::Rc;

/// Stack-friendly internal value type for fast evaluation.
///
/// `Int` and `Float` are fully inline — zero heap allocation.
/// `Str`, `Array`, `Object` use `Rc` for cheap cloning.
#[derive(Debug, Clone)]
pub enum JVal {
    Null,
    Bool(bool),
    Int(i64),
    Float(f64),
    Str(Rc<str>),
    Array(Rc<Vec<JVal>>),
    Object(Rc<BTreeMap<Rc<str>, JVal>>),
}

impl JVal {
    #[inline]
    pub fn is_truthy(&self) -> bool {
        match self {
            JVal::Null => false,
            JVal::Bool(b) => *b,
            JVal::Int(n) => *n != 0,
            JVal::Float(f) => *f != 0.0,
            JVal::Str(s) => !s.is_empty(),
            JVal::Array(a) => !a.is_empty(),
            JVal::Object(o) => !o.is_empty(),
        }
    }

    #[inline]
    pub fn as_i64(&self) -> Option<i64> {
        match self {
            JVal::Int(n) => Some(*n),
            JVal::Float(f) => Some(*f as i64),
            JVal::Bool(b) => Some(if *b { 1 } else { 0 }),
            JVal::Str(s) => s.parse::<i64>().ok(),
            _ => None,
        }
    }

    #[inline]
    pub fn as_f64(&self) -> Option<f64> {
        match self {
            JVal::Int(n) => Some(*n as f64),
            JVal::Float(f) => Some(*f),
            JVal::Bool(b) => Some(if *b { 1.0 } else { 0.0 }),
            JVal::Str(s) => s.parse::<f64>().ok(),
            _ => None,
        }
    }

    #[inline]
    pub fn as_str(&self) -> Option<&str> {
        match self {
            JVal::Str(s) => Some(s.as_ref()),
            _ => None,
        }
    }

    pub fn cmp_numeric_or_string(&self, other: &JVal) -> Option<std::cmp::Ordering> {
        match (self.as_f64(), other.as_f64()) {
            (Some(a), Some(b)) => a.partial_cmp(&b),
            _ => {
                let a = self.display_string();
                let b = other.display_string();
                Some(a.cmp(&b))
            }
        }
    }

    pub fn display_string(&self) -> String {
        match self {
            JVal::Null => "null".to_string(),
            JVal::Bool(b) => b.to_string(),
            JVal::Int(n) => n.to_string(),
            JVal::Float(f) => format_f64(*f),
            JVal::Str(s) => s.to_string(),
            JVal::Array(_) | JVal::Object(_) => {
                let v: Value = self.clone().into();
                v.to_string()
            }
        }
    }
}

impl PartialEq for JVal {
    fn eq(&self, other: &Self) -> bool {
        match (self, other) {
            (JVal::Null, JVal::Null) => true,
            (JVal::Bool(a), JVal::Bool(b)) => a == b,
            (JVal::Int(a), JVal::Int(b)) => a == b,
            (JVal::Float(a), JVal::Float(b)) => a == b,
            (JVal::Int(a), JVal::Float(b)) => (*a as f64) == *b,
            (JVal::Float(a), JVal::Int(b)) => *a == (*b as f64),
            (JVal::Str(a), JVal::Str(b)) => a == b,
            (JVal::Array(a), JVal::Array(b)) => a == b,
            (JVal::Object(a), JVal::Object(b)) => a == b,
            _ => false,
        }
    }
}

pub(crate) fn format_f64(f: f64) -> String {
    if f.fract() == 0.0 && f.is_finite() {
        format!("{:.0}", f)
    } else {
        f.to_string()
    }
}

impl From<Value> for JVal {
    fn from(v: Value) -> Self {
        match v {
            Value::Null => JVal::Null,
            Value::Bool(b) => JVal::Bool(b),
            Value::Number(n) => {
                if let Some(i) = n.as_i64() {
                    JVal::Int(i)
                } else if let Some(f) = n.as_f64() {
                    JVal::Float(f)
                } else {
                    JVal::Null
                }
            }
            Value::String(s) => JVal::Str(Rc::from(s.as_str())),
            Value::Array(arr) => {
                JVal::Array(Rc::new(arr.into_iter().map(JVal::from).collect()))
            }
            Value::Object(obj) => {
                let map: BTreeMap<Rc<str>, JVal> = obj
                    .into_iter()
                    .map(|(k, v)| (Rc::from(k.as_str()), JVal::from(v)))
                    .collect();
                JVal::Object(Rc::new(map))
            }
        }
    }
}

impl From<JVal> for Value {
    fn from(jv: JVal) -> Self {
        match jv {
            JVal::Null => Value::Null,
            JVal::Bool(b) => Value::Bool(b),
            JVal::Int(n) => Value::Number(Number::from(n)),
            JVal::Float(f) => Number::from_f64(f)
                .map(Value::Number)
                .unwrap_or(Value::Null),
            JVal::Str(s) => Value::String(s.to_string()),
            JVal::Array(arr) => Value::Array(
                Rc::try_unwrap(arr)
                    .unwrap_or_else(|rc| (*rc).clone())
                    .into_iter()
                    .map(Value::from)
                    .collect(),
            ),
            JVal::Object(obj) => {
                let map: Map<String, Value> = Rc::try_unwrap(obj)
                    .unwrap_or_else(|rc| (*rc).clone())
                    .into_iter()
                    .map(|(k, v)| (k.to_string(), Value::from(v)))
                    .collect();
                Value::Object(map)
            }
        }
    }
}

// Allow &JVal -> Value conversion too
impl From<&JVal> for Value {
    fn from(jv: &JVal) -> Self {
        jv.clone().into()
    }
}
