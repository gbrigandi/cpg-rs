//! # cpg-rs
//! 
//! A Rust library for working with Code Property Graphs (CPGs).
//! 
//! This crate provides a complete set of data structures for representing Code Property Graphs,
//! a language-agnostic intermediate representation for static code analysis. CPGs combine
//! abstract syntax trees, control flow graphs, and data flow graphs into a unified structure
//! that enables sophisticated program analysis.
//!
//! ## Features
//!
//! - Complete implementation of the CPG specification
//! - Serialization/deserialization support via serde
//! - Strongly-typed enums for node types, edge types, and properties
//! - Support for CPG overlays and diff graphs for incremental analysis
//!
//! ## Usage
//!
//! The main data structures in this crate are:
//!
//! - [`Cpg`]: The root structure containing nodes and edges
//! - [`Node`]: Represents program entities like methods, variables, and expressions
//! - [`Edge`]: Represents relationships between nodes
//! - [`PropertyValue`]: Represents typed property values attached to nodes and edges
//!
//! CPGs can be created programmatically or deserialized from JSON.
//!
//! ## Examples
//!
//! ### Creating and modifying a CPG
//!
//! ```rust
//! use cpg_rs::{Cpg, Node, Edge, NodeType, EdgeType, NodeProperty, NodePropertyName, PropertyValue, PropertyValueEnum};
//! use std::fs::File;
//! use std::io::{BufWriter, BufReader};
//!
//! // Create a new CPG
//! let cpg = Cpg {
//!     node: vec![
//!         Node {
//!             key: 1,
//!             r#type: NodeType::Method,
//!             property: vec![
//!                 NodeProperty {
//!                     name: NodePropertyName::Name,
//!                     value: Some(PropertyValue {
//!                         value: Some(PropertyValueEnum::StringValue("main".to_string())),
//!                     }),
//!                 },
//!             ],
//!         },
//!     ],
//!     edge: vec![],
//! };
//!
//! // Serialize to JSON
//! let file = File::create("cpg.json").unwrap();
//! let writer = BufWriter::new(file);
//! serde_json::to_writer_pretty(writer, &cpg).unwrap();
//!
//! // Deserialize from JSON
//! let file = File::open("cpg.json").unwrap();
//! let reader = BufReader::new(file);
//! let cpg: Cpg = serde_json::from_reader(reader).unwrap();
//! ```
//!
//! See the [examples directory](https://github.com/gbrigandi/cpg-rs/tree/main/examples) for more examples:
//!
//! - `modify_cpg.rs`: Creating, modifying, and saving a CPG
//! - `find_methods.rs`: Finding methods and parameters in a CPG
//! - `diff_graph.rs`: Working with diff graphs to represent changes to a CPG
//!
//! See the documentation for individual types for more details.


use serde::{Deserialize, Serialize};

/// Represents a typed property value that can be attached to nodes and edges in a CPG.
/// 
/// Property values can be of various types like strings, numbers, booleans, or lists.
/// The actual value is stored in the `value` field as a `PropertyValueEnum`.
#[derive(Serialize, Deserialize, Clone, Debug, PartialEq)]
pub struct PropertyValue {
    #[serde(flatten)]
    pub value: Option<PropertyValueEnum>,
}

/// Represents the different types of values that a property can have in a CPG.
/// 
/// This enum uses serde's tagged representation with "type" and "value" fields
/// to support proper serialization and deserialization to/from JSON.
#[derive(Serialize, Deserialize, Clone, Debug, PartialEq)]
#[serde(tag = "type", content = "value")]
pub enum PropertyValueEnum {
    #[serde(rename = "string_value")]
    StringValue(String),
    #[serde(rename = "bool_value")]
    BoolValue(bool),
    #[serde(rename = "int_value")]
    IntValue(i32),
    #[serde(rename = "long_value")]
    LongValue(i64),
    #[serde(rename = "float_value")]
    FloatValue(f32),
    #[serde(rename = "double_value")]
    DoubleValue(f64),
    #[serde(rename = "string_list")]
    StringList(StringList),
    #[serde(rename = "bool_list")]
    BoolList(BoolList),
    #[serde(rename = "int_list")]
    IntList(IntList),
    #[serde(rename = "long_list")]
    LongList(LongList),
    #[serde(rename = "float_list")]
    FloatList(FloatList),
    #[serde(rename = "double_list")]
    DoubleList(DoubleList),
    #[serde(rename = "contained_refs")]
    ContainedRefs(ContainedRefs),
}

/// Represents references to other nodes in the CPG.
/// 
/// This is used to establish relationships between nodes without using edges.
#[derive(Serialize, Deserialize, Clone, Debug, PartialEq)]
pub struct ContainedRefs {
    pub local_name: String,
    pub refs: Vec<i64>,
}

/// A list of string values that can be used as a property value.
#[derive(Serialize, Deserialize, Clone, Debug, PartialEq)]
pub struct StringList {
    pub values: Vec<String>,
}

/// A list of boolean values that can be used as a property value.
#[derive(Serialize, Deserialize, Clone, Debug, PartialEq)]
pub struct BoolList {
    pub values: Vec<bool>,
}

/// A list of 32-bit integer values that can be used as a property value.
#[derive(Serialize, Deserialize, Clone, Debug, PartialEq)]
pub struct IntList {
    pub values: Vec<i32>,
}

/// A list of 64-bit integer values that can be used as a property value.
#[derive(Serialize, Deserialize, Clone, Debug, PartialEq)]
pub struct LongList {
    pub values: Vec<i64>,
}

/// A list of 32-bit floating point values that can be used as a property value.
#[derive(Serialize, Deserialize, Clone, Debug, PartialEq)]
pub struct FloatList {
    pub values: Vec<f32>,
}

/// A list of 64-bit floating point values that can be used as a property value.
#[derive(Serialize, Deserialize, Clone, Debug, PartialEq)]
pub struct DoubleList {
    pub values: Vec<f64>,
}

/// The root structure of a Code Property Graph (CPG).
/// 
/// A CPG consists of nodes representing program entities (methods, variables, etc.)
/// and edges representing relationships between those entities.
/// 
/// # Examples
/// 
/// ```
/// use cpg_rs::{Cpg, Node, Edge, NodeType, EdgeType};
/// 
/// // Create a new CPG with nodes and edges
/// let cpg = Cpg {
///     node: vec![], // Add nodes here
///     edge: vec![], // Add edges here
/// };
/// ```
#[derive(Serialize, Deserialize, Clone, Debug, PartialEq)]
pub struct Cpg {
    pub node: Vec<Node>,
    pub edge: Vec<Edge>,
}

/// Represents a node in a Code Property Graph.
/// 
/// Nodes represent program entities like methods, variables, expressions, etc.
/// Each node has a unique key, a type, and a list of properties.
/// 
/// # Examples
/// 
/// ```
/// use cpg_rs::{Node, NodeType, NodeProperty, NodePropertyName, PropertyValue, PropertyValueEnum};
/// 
/// // Create a method node
/// let method_node = Node {
///     key: 1,
///     r#type: NodeType::Method,
///     property: vec![
///         NodeProperty {
///             name: NodePropertyName::Name,
///             value: Some(PropertyValue {
///                 value: Some(PropertyValueEnum::StringValue("main".to_string())),
///             }),
///         },
///     ],
/// };
/// ```
#[derive(Serialize, Deserialize, Clone, Debug, PartialEq)]
pub struct Node {
    pub key: i64,
    pub r#type: NodeType,
    pub property: Vec<NodeProperty>,
}

/// Represents a property of a node in a Code Property Graph.
/// 
/// Each property has a name and an optional value.
#[derive(Serialize, Deserialize, Clone, Debug, PartialEq)]
pub struct NodeProperty {
    pub name: NodePropertyName,
    pub value: Option<PropertyValue>,
}

#[derive(Serialize, Deserialize, Clone, Copy, Debug, PartialEq, Eq, Hash, PartialOrd, Ord)]
pub enum NodeType {
    #[serde(rename = "UNKNOWN_NODE_TYPE")]
    UnknownNodeType = 0,
    /// Programming languages offer many closely-related concepts for describing blocks
    /// of code that can be executed with input parameters and return output parameters,
    /// possibly causing side effects. In the CPG specification, we refer to all of these
    /// concepts (procedures, functions, methods, etc.) as methods. A single METHOD node
    /// must exist for each method found in the source program.
    ///
    /// The `FULL_NAME` field specifies the method's fully-qualified name, including
    /// information about the namespace it is contained in if applicable, the name field
    /// is the function's short name. The field `IS_EXTERNAL` indicates whether it was
    /// possible to identify a method body for the method. This is true for methods that
    /// are defined in the source program, and false for methods that are dynamically
    /// linked to the program, that is, methods that exist in an external dependency.
    ///
    /// Line and column number information is specified in the optional fields
    /// `LINE_NUMBER`, `COLUMN_NUMBER`, `LINE_NUMBER_END`, and `COLUMN_NUMBER_END` and
    /// the name of the source file is specified in `FILENAME`. An optional hash value
    /// MAY be calculated over the function contents and included in the `HASH` field.
    ///
    /// Finally, the fully qualified name of the program constructs that the method
    /// is immediately contained in is stored in the `AST_PARENT_FULL_NAME` field
    /// and its type is indicated in the `AST_PARENT_TYPE` field to be one of
    /// `METHOD`, `TYPE_DECL` or `NAMESPACE_BLOCK`.
    #[serde(rename = "METHOD")]
    Method = 1,
    /// This node represents an (unnamed) formal method return parameter. It carries its
    /// fully qualified type name in `TYPE_FULL_NAME`. The `CODE` field MAY be set freely,
    /// e.g., to the constant `RET`, however, subsequent layer creators MUST NOT depend
    /// on this value.
    #[serde(rename = "METHOD_RETURN")]
    MethodReturn = 3,
    /// A method annotation.
    /// The semantics of the FULL_NAME property on this node differ from the usual FULL_NAME
    /// semantics in the sense that FULL_NAME describes the represented annotation class/interface
    /// itself and not the ANNOTATION node.
    #[serde(rename = "ANNOTATION")]
    Annotation = 5,
    /// Assignment of annotation argument to annotation parameter
    #[serde(rename = "ANNOTATION_PARAMETER_ASSIGN")]
    AnnotationParameterAssign = 6,
    /// Formal annotation parameter
    #[serde(rename = "ANNOTATION_PARAMETER")]
    AnnotationParameter = 7,
    /// This node represents a literal such as an integer or string constant. Literals
    /// are symbols included in the code in verbatim form and which are immutable.
    /// The `TYPE_FULL_NAME` field stores the literal's fully-qualified type name,
    /// e.g., `java.lang.Integer`.
    #[serde(rename = "LITERAL")]
    Literal = 8,
    /// This node represents a type member of a class, struct or union, e.g., for the
    /// type declaration `class Foo{ int i ; }`, it represents the declaration of the
    /// variable `i`.
    #[serde(rename = "MEMBER")]
    Member = 9,
    /// Initialization construct for arrays
    #[serde(rename = "ARRAY_INITIALIZER")]
    ArrayInitializer = 14,
    /// A (function/method/procedure) call. The `METHOD_FULL_NAME` property is the name of the
    /// invoked method (the callee) while the `TYPE_FULL_NAME` is its return type, and
    /// therefore, the return type of the call when viewing it as an expression. For
    /// languages like Javascript, it is common that we may know the (short-) name
    /// of the invoked method, but we do not know at compile time which method
    /// will actually be invoked, e.g., because it depends on a dynamic import.
    /// In this case, we leave `METHOD_FULL_NAME` blank but at least fill out `NAME`,
    /// which contains the method's (short-) name and `SIGNATURE`, which contains
    /// any information we may have about the types of arguments and return value.
    #[serde(rename = "CALL")]
    Call = 15,
    /// This node represents a local variable. Its fully qualified type name is stored
    /// in the `TYPE_FULL_NAME` field and its name in the `NAME` field. The `CODE` field
    /// contains the entire local variable declaration without initialization, e.g., for
    /// `int x = 10;`, it contains `int x`.
    #[serde(rename = "LOCAL")]
    Local = 23,
    /// This node represents a tag.
    #[serde(rename = "TAG")]
    Tag = 24,
    /// A location node summarizes a source code location.
    #[serde(rename = "LOCATION")]
    Location = 25,
    /// This node represents an identifier as used when referring to a variable by name.
    /// It holds the identifier's name in the `NAME` field and its fully-qualified type
    /// name in `TYPE_FULL_NAME`.
    #[serde(rename = "IDENTIFIER")]
    Identifier = 27,
    /// This node represents a return instruction, e.g., `return x`. Note that it does
    /// NOT represent a formal return parameter as formal return parameters are
    /// represented via `METHOD_RETURN` nodes.
    #[serde(rename = "RETURN")]
    Return = 30,
    /// This node represents a compound statement. Compound statements are used in many languages to allow
    /// grouping a sequence of statements. For example, in C and Java, compound statements
    /// are statements enclosed by curly braces. Function/Method bodies are compound
    /// statements. We do not use the term "compound statement" because "statement" would
    /// imply that the block does not yield a value upon evaluation, that is, that it is
    /// not an expression. This is true in languages such as C and Java, but not for languages
    /// such as Scala where the value of the block is given by that of the last expression it
    /// contains. In fact, the Scala grammar uses the term "BlockExpr" (short for
    /// "block expression") to describe what in the CPG we call "Block".
    #[serde(rename = "BLOCK")]
    Block = 31,
    /// This node represents a formal output parameter. Corresponding output parameters
    /// for input parameters MUST NOT be created by the frontend as they are automatically
    /// created upon first loading the CPG.
    #[serde(rename = "METHOD_PARAMETER_OUT")]
    MethodParameterOut = 33,
    /// This node represents a formal input parameter. The field `NAME` contains its
    /// name, while the field `TYPE_FULL_NAME` contains the fully qualified type name.
    #[serde(rename = "METHOD_PARAMETER_IN")]
    MethodParameterIn = 34,
    /// This node represents a dependency
    #[serde(rename = "DEPENDENCY")]
    Dependency = 35,
    /// File nodes represent source files or a shared objects from which the CPG
    /// was generated. File nodes serve as indices, that is, they allow looking up all
    /// elements of the code by file.
    ///
    /// For each file, the graph CAN contain exactly one File node, if not File nodes
    /// are created as indicated by `FILENAME` property of other nodes.
    /// As file nodes are root nodes of abstract syntax tress, they are AstNodes and
    /// their order field is set to 0. This is because they have no sibling nodes,
    /// not because they are the first node of the AST.
    #[serde(rename = "FILE")]
    File = 38,
    /// This node contains the CPG meta data. Exactly one node of this type
    /// MUST exist per CPG. The `HASH` property MAY contain a hash value calculated
    /// over the source files this CPG was generated from. The `VERSION` MUST be
    /// set to the version of the specification ("1.1"). The language field indicates
    /// which language frontend was used to generate the CPG and the list property
    /// `OVERLAYS` specifies which overlays have been applied to the CPG.
    #[serde(rename = "META_DATA")]
    MetaData = 39,
    /// This node represents a namespace. Similar to FILE nodes, NAMESPACE nodes
    /// serve as indices that allow all definitions inside a namespace to be
    /// obtained by following outgoing edges from a NAMESPACE node.
    ///
    /// NAMESPACE nodes MUST NOT be created by language frontends. Instead,
    /// they are generated from NAMESPACE_BLOCK nodes automatically upon
    /// first loading of the CPG.
    #[serde(rename = "NAMESPACE")]
    Namespace = 40,
    /// A reference to a namespace.
    /// We borrow the concept of a "namespace block" from C++, that is, a namespace block
    /// is a block of code that has been placed in the same namespace by a programmer.
    /// This block may be introduced via a `package` statement in Java or
    /// a `namespace{ }` statement in C++.
    ///
    /// The `FULL_NAME` field contains a unique identifier to represent the namespace block
    /// itself not just the namespace it references. So in addition to the namespace name
    /// it can be useful to use the containing file name to derive a unique identifier.
    ///
    /// The `NAME` field contains the namespace name in a human-readable format.
    /// The name should be given in dot-separated form where a dot indicates
    /// that the right hand side is a sub namespace of the left hand side, e.g.,
    /// `foo.bar` denotes the namespace `bar` contained in the namespace `foo`.
    #[serde(rename = "NAMESPACE_BLOCK")]
    NamespaceBlock = 41,
    /// Any AST node that the frontend would like to include in the AST but for
    /// which no suitable AST node is specified in the CPG specification may be
    /// included using a node of type `UNKNOWN`.
    #[serde(rename = "UNKNOWN")]
    Unknown = 44,
    /// This node represents a type instance, that is, a concrete instantiation
    /// of a type declaration.
    #[serde(rename = "TYPE")]
    Type = 45,
    /// This node represents a type declaration as for example given by a class-, struct-,
    /// or union declaration. In contrast to a `TYPE` node, this node does not represent a
    /// concrete instantiation of a type, e.g., for the parametrized type `List\[T\]`, it represents
    /// `List\[T\]`, but not `List\[Integer\]` where `Integer` is a concrete type.
    ///
    /// The language frontend MUST create type declarations for all types declared in the
    /// source program and MAY provide type declarations for types that are not declared
    /// but referenced by the source program. If a declaration is present in the source
    /// program, the field `IS_EXTERNAL` is set to `false`. Otherwise, it is set to `true`.
    ///
    /// The `FULL_NAME` field specifies the type's fully-qualified name, including
    /// information about the namespace it is contained in if applicable, the name field
    /// is the type's short name. Line and column number information is specified in the
    /// optional fields `LINE_NUMBER`, `COLUMN_NUMBER`, `LINE_NUMBER_END`, and
    /// `COLUMN_NUMBER_END` and the name of the source file is specified in `FILENAME`.
    ///
    /// Base types can be specified via the `INHERITS_FROM_TYPE_FULL_NAME` list, where
    /// each entry contains the fully-qualified name of a base type. If the type is
    /// known to be an alias of another type (as for example introduced via the C
    /// `typedef` statement), the name of the alias is stored in `ALIAS_TYPE_FULL_NAME`.
    ///
    /// Finally, the fully qualified name of the program constructs that the type declaration
    /// is immediately contained in is stored in the `AST_PARENT_FULL_NAME` field
    /// and its type is indicated in the `AST_PARENT_TYPE` field to be one of
    /// `METHOD`, `TYPE_DECL` or `NAMESPACE_BLOCK`.
    #[serde(rename = "TYPE_DECL")]
    TypeDecl = 46,
    /// This node represents a formal type parameter, that is, the type parameter
    /// as given in a type-parametrized method or type declaration. Examples for
    /// languages that support type parameters are Java (via Generics) and C++
    /// (via templates). Apart from the standard fields of AST nodes, the type
    /// parameter carries only a `NAME` field that holds the parameters name.
    #[serde(rename = "TYPE_PARAMETER")]
    TypeParameter = 47,
    /// An (actual) type argument as used to instantiate a parametrized type, in the
    /// same way an (actual) arguments provides concrete values for a parameter
    /// at method call sites. As it true for arguments, the method is not expected
    /// to  interpret the type argument. It MUST however store its code in the
    /// `CODE` field.
    #[serde(rename = "TYPE_ARGUMENT")]
    TypeArgument = 48,
    /// A literal value assigned to an ANNOTATION_PARAMETER
    #[serde(rename = "ANNOTATION_LITERAL")]
    AnnotationLiteral = 49,
    /// This node type represent a configuration file, where `NAME` is the name
    /// of the file and `content` is its content. The exact representation of the
    /// name is left undefined and can be chosen as required by consumers of
    /// the corresponding configuration files.
    #[serde(rename = "CONFIG_FILE")]
    ConfigFile = 50,
    /// `BINDING` nodes represent name-signature pairs that can be resolved at a
    /// type declaration (`TYPE_DECL`). They are connected to `TYPE_DECL` nodes via
    /// incoming `BINDS` edges. The bound method is either associated with an outgoing
    /// `REF` edge to a `METHOD` or with the `METHOD_FULL_NAME` property. The `REF` edge
    /// if present has priority.
    #[serde(rename = "BINDING")]
    Binding = 146,
    /// This node contains an arbitrary node and an associated tag node.
    #[serde(rename = "TAG_NODE_PAIR")]
    TagNodePair = 208,
    /// Finding nodes may be used to store analysis results in the graph
    /// that are to be exposed to an end-user, e.g., information about
    /// potential vulnerabilities or dangerous programming practices.
    /// A Finding node may contain an abitrary list of key value pairs
    /// that characterize the finding, as well as a list of nodes that
    /// serve as evidence for the finding.
    #[serde(rename = "FINDING")]
    Finding = 214,
    /// This node represents a key value pair, where both the key and the value are strings.
    #[serde(rename = "KEY_VALUE_PAIR")]
    KeyValuePair = 217,
    /// This field represents a (language-dependent) modifier such as `static`, `private`
    /// or `public`. Unlike most other AST nodes, it is NOT an expression, that is, it
    /// cannot be evaluated and cannot be passed as an argument in function calls.
    #[serde(rename = "MODIFIER")]
    Modifier = 300,
    /// This node represents a reference to a method/function/procedure as it
    /// appears when a method is passed as an argument in a call. The `METHOD_FULL_NAME`
    /// field holds the fully-qualified name of the referenced method and the
    /// `TYPE_FULL_NAME` holds its fully-qualified type name.
    #[serde(rename = "METHOD_REF")]
    MethodRef = 333,
    /// Represents the binding of a LOCAL or METHOD_PARAMETER_IN into the closure of a method
    #[serde(rename = "CLOSURE_BINDING")]
    ClosureBinding = 334,
    /// Reference to a type/class
    #[serde(rename = "TYPE_REF")]
    TypeRef = 335,
    /// This node represents a control structure as introduced by control structure
    /// statements as well as conditional and unconditional jumps. Its type is stored in the
    /// `CONTROL_STRUCTURE_TYPE` field to be one of several pre-defined types. These types
    /// are used in the construction of the control flow layer, making it possible to
    /// generate the control flow layer from the abstract syntax tree layer automatically.
    ///
    /// In addition to the `CONTROL_STRUCTURE_TYPE` field, the `PARSER_TYPE_NAME` field
    /// MAY be used by frontends to store the name of the control structure as emitted by
    /// the parser or disassembler, however, the value of this field is not relevant
    /// for construction of the control flow layer.
    #[serde(rename = "CONTROL_STRUCTURE")]
    ControlStructure = 339,
    /// A jump target is any location in the code that has been specifically marked
    /// as the target of a jump, e.g., via a label. The `NAME` field holds the name of
    /// the label while the `PARSER_TYPE_NAME` field holds the name of language construct
    /// that this jump target is created from, e.g., "Label".
    #[serde(rename = "JUMP_TARGET")]
    JumpTarget = 340,
    /// A jump label specifies the label and thus the JUMP_TARGET of control structures
    /// BREAK and CONTINUE. The `NAME` field holds the name of the label while the
    /// `PARSER_TYPE_NAME` field holds the name of language construct that this jump
    /// label is created from, e.g., "Label".
    #[serde(rename = "JUMP_LABEL")]
    JumpLabel = 341,
    /// This node represents a DOM node used in template languages, e.g., JSX/TSX
    #[serde(rename = "TEMPLATE_DOM")]
    TemplateDom = 417,
    /// A source code comment
    #[serde(rename = "COMMENT")]
    Comment = 511,
    /// This node represents the field accessed in a field access, e.g., in
    /// `a.b`, it represents `b`. The field name as it occurs in the code is
    /// stored in the `CODE` field. This may mean that the `CODE` field holds
    /// an expression. The `CANONICAL_NAME` field MAY contain the same value is
    /// the `CODE` field but SHOULD contain the normalized name that results
    /// from evaluating `CODE` as an expression if such an evaluation is
    /// possible for the language frontend. The objective is to store an identifier
    /// in `CANONICAL_NAME` that is the same for two nodes iff they refer to the
    /// same field, regardless of whether they use the same expression to reference
    /// it.
    #[serde(rename = "FIELD_IDENTIFIER")]
    FieldIdentifier = 2001081,
}

/// Represents an edge in a Code Property Graph.
/// 
/// Edges represent relationships between nodes, such as method calls,
/// control flow, data flow, etc. Each edge has a source node, a destination node,
/// a type, and optional properties.
/// 
/// # Examples
/// 
/// ```
/// use cpg_rs::{Edge, EdgeType};
/// 
/// // Create an edge from node 1 to node 2
/// let edge = Edge {
///     src: 1,
///     dst: 2,
///     r#type: EdgeType::Ast,
///     property: vec![],
/// };
/// ```
#[derive(Serialize, Deserialize, Clone, Debug, PartialEq)]
pub struct Edge {
    /// Source node.
    pub src: i64,
    /// Destination node.
    pub dst: i64,
    pub r#type: EdgeType,
    pub property: Vec<EdgeProperty>,
}

/// Represents a property of an edge in a Code Property Graph.
/// 
/// Each property has a name and an optional value.
#[derive(Serialize, Deserialize, Clone, Debug, PartialEq)]
pub struct EdgeProperty {
    pub name: EdgePropertyName,
    pub value: Option<PropertyValue>,
}

#[derive(Serialize, Deserialize, Clone, Copy, Debug, PartialEq, Eq, Hash, PartialOrd, Ord)]
pub enum EdgeType {
    #[serde(rename = "UNKNOWN_EDGE_TYPE")]
    UnknownEdgeType = 0,
    /// This edge connects a parent node to its child in the syntax tree.
    #[serde(rename = "AST")]
    Ast = 3,
    /// This edge connects call sites, i.e., nodes with the type `CALL`, to the
    /// method node that represent the method they invoke. The frontend MAY create
    /// `CALL` edges but is not required to do so. Instead, of the `METHOD_FULL_NAME`
    /// field of the `CALL` node is set correctly, `CALL` edges are created
    /// automatically as the CPG is first loaded.
    #[serde(rename = "CALL")]
    Call = 6,
    /// This edge indicates that the source node is an identifier that denotes
    /// access to the destination node. For example, an identifier may reference
    /// a local variable.
    #[serde(rename = "REF")]
    Ref = 10,
    /// Edges from nodes to the tags they are tagged by.
    #[serde(rename = "TAGGED_BY")]
    TaggedBy = 11,
    /// This edge connects a method input parameter to the corresponding
    /// method output parameter.
    #[serde(rename = "PARAMETER_LINK")]
    ParameterLink = 12,
    /// This edge indicates control flow from the source to the destination node.
    #[serde(rename = "CFG")]
    Cfg = 19,
    /// This edge connects a node to its evaluation type.
    #[serde(rename = "EVAL_TYPE")]
    EvalType = 21,
    /// This edge connects type arguments to type parameters to indicate
    /// that the type argument is used to instantiate the type parameter.
    #[serde(rename = "BINDS_TO")]
    BindsTo = 22,
    /// Inheritance relation between a type declaration and a type. This edge MUST NOT
    /// be created by the language frontend as it is automatically created from
    /// `INHERITS_FROM_TYPE_FULL_NAME` fields then the CPG is first loaded.
    #[serde(rename = "INHERITS_FROM")]
    InheritsFrom = 23,
    /// This edge connects a node to the method that contains it.
    #[serde(rename = "CONTAINS")]
    Contains = 28,
    /// Represents the capturing of a variable into a closure
    #[serde(rename = "CAPTURE")]
    Capture = 40,
    /// Connection between a captured LOCAL and the corresponding CLOSURE_BINDING
    #[serde(rename = "CAPTURED_BY")]
    CapturedBy = 41,
    /// Similar to `ARGUMENT` edges, `RECEIVER` edges connect call sites
    /// to their receiver arguments. A receiver argument is the object on
    /// which a method operates, that is, it is the expression that is
    /// assigned to the `this` pointer as control is transferred to the method.
    #[serde(rename = "RECEIVER")]
    Receiver = 55,
    /// The edge connects control structure nodes to the expressions that holds their conditions.
    #[serde(rename = "CONDITION")]
    Condition = 56,
    /// A reaching definition edge indicates that a variable produced at the source node reaches
    /// the destination node without being reassigned on the way. The `VARIABLE` property indicates
    /// which variable is propagated.
    #[serde(rename = "REACHING_DEF")]
    ReachingDef = 137,
    /// This edge represents an alias relation between a type declaration and a type.
    /// The language frontend MUST NOT create `ALIAS_OF` edges as they are created
    /// automatically based on `ALIAS_TYPE_FULL_NAME` fields when the CPG is first loaded.
    #[serde(rename = "ALIAS_OF")]
    AliasOf = 138,
    /// This edge connects a type declaration (`TYPE_DECL`) with a binding node (`BINDING`) and
    /// indicates that the type declaration has the binding represented by the binding node, in
    /// other words, there is a (name, signature) pair that can be resolved for the type
    /// declaration as stored in the binding node.
    #[serde(rename = "BINDS")]
    Binds = 155,
    /// Argument edges connect call sites (node type `CALL`) to their arguments
    /// (node type `EXPRESSION`) as well as `RETURN` nodes to the expressions
    /// that return.
    #[serde(rename = "ARGUMENT")]
    Argument = 156,
    /// This edge connects a node to the node that represents its source file. These
    /// edges MUST not be created by the language frontend but are automatically
    /// created based on `FILENAME` fields.
    #[serde(rename = "SOURCE_FILE")]
    SourceFile = 157,
    /// This edge indicates that the source node immediately dominates the destination node.
    #[serde(rename = "DOMINATE")]
    Dominate = 181,
    /// This edge indicates that the source node immediately post dominates the destination node.
    #[serde(rename = "POST_DOMINATE")]
    PostDominate = 182,
    /// A CDG edge expresses that the destination node is control dependent on the source node.
    #[serde(rename = "CDG")]
    Cdg = 183,
    /// Edge from imports to dependencies
    #[serde(rename = "IMPORTS")]
    Imports = 23663,
    /// Edge from CALL statement in the AST to the IMPORT.
    /// ￼        |We use this edge to traverse from the logical representation of the IMPORT
    /// ￼        |to the corresponding import statement in the AST.
    /// ￼        |
    #[serde(rename = "IS_CALL_FOR_IMPORT")]
    IsCallForImport = 23664,
}

#[derive(Serialize, Deserialize, Clone, Debug, PartialEq)]
pub struct AdditionalNodeProperty {
    pub node_id: i64,
    pub property: Option<NodeProperty>,
}

#[derive(Serialize, Deserialize, Clone, Debug, PartialEq)]
pub struct AdditionalEdgeProperty {
    pub edge_id: i64,
    pub property: Option<EdgeProperty>,
    pub out_node_key: i64,
    pub in_node_key: i64,
    pub edge_type: EdgeType,
}

/// Overlays can be stacked onto each other, therefore their node ids must be globally unique.
#[derive(Serialize, Deserialize, Clone, Debug, PartialEq)]
pub struct CpgOverlay {
    pub node: Vec<Node>,
    pub edge: Vec<Edge>,
    pub node_property: Vec<AdditionalNodeProperty>,
    pub edge_property: Vec<AdditionalEdgeProperty>,
}

/// Represents a set of changes to be applied to a Code Property Graph.
/// 
/// DiffGraphs can be created independently of each other and therefor when _adding_ nodes|edges,
/// each DiffGraph has its own ID space. However, when removing nodes|edges, the nodeIds refer to the
/// globally unique graph id space.
#[derive(Serialize, Deserialize, Clone, Debug, PartialEq)]
pub struct DiffGraph {
    pub entries: Vec<DiffGraphEntry>,
}

#[derive(Serialize, Deserialize, Clone, Debug, PartialEq)]
#[serde(tag = "type", content = "content")]
pub enum DiffGraphEntry {
    #[serde(rename = "node")]
    Node(Node),
    #[serde(rename = "edge")]
    Edge(Edge),
    #[serde(rename = "node_property")]
    NodeProperty(AdditionalNodeProperty),
    #[serde(rename = "edge_property")]
    EdgeProperty(AdditionalEdgeProperty),
    #[serde(rename = "remove_node")]
    RemoveNode(RemoveNode),
    #[serde(rename = "remove_node_property")]
    RemoveNodeProperty(RemoveNodeProperty),
    #[serde(rename = "remove_edge")]
    RemoveEdge(RemoveEdge),
    #[serde(rename = "remove_edge_property")]
    RemoveEdgeProperty(RemoveEdgeProperty),
}

#[derive(Serialize, Deserialize, Clone, Debug, PartialEq)]
pub struct RemoveNode {
    pub key: i64,
}

#[derive(Serialize, Deserialize, Clone, Debug, PartialEq)]
pub struct RemoveNodeProperty {
    pub key: i64,
    pub name: NodePropertyName,
    pub local_name: String,
}

#[derive(Serialize, Deserialize, Clone, Debug, PartialEq)]
pub struct RemoveEdge {
    pub out_node_key: i64,
    pub in_node_key: i64,
    pub edge_type: EdgeType,
    /// used to identify edges (since our edges don't have ids)
    pub properties_hash: Vec<u8>,
}

#[derive(Serialize, Deserialize, Clone, Debug, PartialEq)]
pub struct RemoveEdgeProperty {
    pub out_node_key: i64,
    pub in_node_key: i64,
    pub edge_type: EdgeType,
    /// used to identify edges (since our edges don't have ids)
    pub properties_hash: Vec<u8>,
    pub property_name: EdgePropertyName,
}

#[derive(Serialize, Deserialize, Clone, Copy, Debug, PartialEq, Eq, Hash, PartialOrd, Ord)]
pub enum NodePropertyName {
    #[serde(rename = "UNKNOWN_NODE_PROPERTY")]
    UnknownNodeProperty = 0,
    /// This optional field provides the line number of the program construct
    /// represented by the node.
    #[serde(rename = "LINE_NUMBER")]
    LineNumber = 2,
    /// AST node type name emitted by parser.
    #[serde(rename = "PARSER_TYPE_NAME")]
    ParserTypeName = 3,
    /// This integer indicates the position of the node among
    /// its siblings in the AST. The left-most child has an
    /// order of 0.
    #[serde(rename = "ORDER")]
    Order = 4,
    /// Name of represented object, e.g., method name (e.g. "run")
    #[serde(rename = "NAME")]
    Name = 5,
    /// This is the fully-qualified name of an entity, e.g., the fully-qualified
    /// name of a method or type. The details of what constitutes a fully-qualified
    /// name are language specific. This field SHOULD be human readable.
    #[serde(rename = "FULL_NAME")]
    FullName = 6,
    /// Indicates that the construct (METHOD or TYPE_DECL) is external, that is,
    /// it is referenced but not defined in the code (applies both to insular
    /// parsing and to library functions where we have header files only)
    #[serde(rename = "IS_EXTERNAL")]
    IsExternal = 7,
    /// This property denotes a string value as used in a key-value pair.
    #[serde(rename = "VALUE")]
    Value = 8,
    /// This optional fields provides the column number of the program construct
    /// represented by the node.
    #[serde(rename = "COLUMN_NUMBER")]
    ColumnNumber = 11,
    /// This optional fields provides the line number at which the program construct
    /// represented by the node ends.
    #[serde(rename = "LINE_NUMBER_END")]
    LineNumberEnd = 12,
    /// A version, given as a string. Used, for example, in the META_DATA node to
    /// indicate which version of the CPG spec this CPG conforms to
    #[serde(rename = "VERSION")]
    Version = 13,
    /// For formal method input parameters, output parameters, and return parameters,
    /// this field holds the evaluation strategy, which is one of the following:
    /// 1) `BY_REFERENCE` indicates that the parameter is passed by reference, 2)
    /// `BY_VALUE` indicates that it is passed by value, that is, a copy is made,
    /// 3) `BY_SHARING` the parameter is a pointer/reference and it is shared with
    /// the caller/callee. While a copy of the pointer is made, a copy of the object
    /// that it points to is not made.
    #[serde(rename = "EVALUATION_STRATEGY")]
    EvaluationStrategy = 15,
    /// This optional fields provides the column number at which the program construct
    /// represented by the node ends.
    #[serde(rename = "COLUMN_NUMBER_END")]
    ColumnNumberEnd = 16,
    /// This field indicates which CPG language frontend generated the CPG.
    /// Frontend developers may freely choose a value that describes their frontend
    /// so long as it is not used by an existing frontend. Reserved values are to date:
    /// C, LLVM, GHIDRA, PHP.
    #[serde(rename = "LANGUAGE")]
    Language = 19,
    /// Certain files, e.g., configuration files, may be included in the CPG as-is.
    /// For such files, the `CONTENT` field contains the files content.
    #[serde(rename = "CONTENT")]
    Content = 20,
    /// This field holds the code snippet that the node represents.
    #[serde(rename = "CODE")]
    Code = 21,
    /// The method signature encodes the types of parameters in a string.
    /// The string SHOULD be human readable and suitable for differentiating methods
    /// with different parameter types sufficiently to allow for resolving of
    /// function overloading. The present specification does not enforce a strict
    /// format for the signature, that is, it can be chosen by the frontend
    /// implementor to fit the source language.
    #[serde(rename = "SIGNATURE")]
    Signature = 22,
    /// This field holds the dispatch type of a call, which is either `STATIC_DISPATCH` or
    /// `DYNAMIC_DISPATCH`. For statically dispatched method calls, the call target is known
    /// at compile time while for dynamically dispatched calls, it can only be determined at
    /// runtime as it may depend on the type of an object (as is the case for virtual method
    /// calls) or calculation of an offset.
    #[serde(rename = "DISPATCH_TYPE")]
    DispatchType = 25,
    /// The modifier type is a free-form string. The following are known modifier types:
    /// `STATIC`, `PUBLIC`, `PROTECTED`, `PRIVATE`, `ABSTRACT`, `NATIVE`, `CONSTRUCTOR`, `VIRTUAL`.
    #[serde(rename = "MODIFIER_TYPE")]
    ModifierType = 26,
    /// The `CONTROL_STRUCTURE_TYPE` field indicates which kind of control structure
    /// a `CONTROL_STRUCTURE` node represents. The available types are the following:
    /// BREAK, CONTINUE, DO, WHILE, FOR, GOTO, IF, ELSE, TRY, THROW and SWITCH.
    #[serde(rename = "CONTROL_STRUCTURE_TYPE")]
    ControlStructureType = 27,
    /// AST-children of CALL nodes have an argument index, that is used to match
    /// call-site arguments with callee parameters. Explicit parameters are numbered
    /// from 1 to N, while index 0 is reserved for implicit self / this parameter.
    /// CALLs without implicit parameter therefore have arguments starting with index 1.
    /// AST-children of BLOCK nodes may have an argument index as well; in this case,
    /// the last argument index determines the return expression of a BLOCK expression.
    /// If the `PARAMETER_NAME` field is set, then the `ARGUMENT_INDEX` field is
    /// ignored. It is suggested to set it to -1.
    #[serde(rename = "ARGUMENT_INDEX")]
    ArgumentIndex = 40,
    /// Identifier which uniquely describes a CLOSURE_BINDING. This property is used to match captured LOCAL nodes with the corresponding CLOSURE_BINDING nodes
    #[serde(rename = "CLOSURE_BINDING_ID")]
    ClosureBindingId = 50,
    /// This field contains the fully-qualified static type name of the program
    /// construct represented by a node. It is the name of an instantiated type, e.g.,
    /// `java.util.List<Integer>`, rather than `java.util.List\[T\]`. If the type
    /// cannot be determined, this field should be set to the empty string.
    #[serde(rename = "TYPE_FULL_NAME")]
    TypeFullName = 51,
    /// The static type decl of a TYPE. This property is matched against the FULL_NAME
    /// of TYPE_DECL nodes. It is required to have exactly one TYPE_DECL for each
    /// different TYPE_DECL_FULL_NAME
    #[serde(rename = "TYPE_DECL_FULL_NAME")]
    TypeDeclFullName = 52,
    /// The static types a TYPE_DECL inherits from. This property is matched against the
    /// FULL_NAME of TYPE nodes and thus it is required to have at least one TYPE node
    /// for each TYPE_FULL_NAME
    #[serde(rename = "INHERITS_FROM_TYPE_FULL_NAME")]
    InheritsFromTypeFullName = 53,
    /// The FULL_NAME of a method. Used to link CALL and METHOD nodes. It is required
    /// to have exactly one METHOD node for each METHOD_FULL_NAME
    #[serde(rename = "METHOD_FULL_NAME")]
    MethodFullName = 54,
    /// The type of the AST parent. Since this is only used in some parts of the graph,
    /// the list does not include all possible parents by intention.
    /// Possible parents: METHOD, TYPE_DECL, NAMESPACE_BLOCK.
    #[serde(rename = "AST_PARENT_TYPE")]
    AstParentType = 56,
    /// This field holds the FULL_NAME of the AST parent of an entity.
    #[serde(rename = "AST_PARENT_FULL_NAME")]
    AstParentFullName = 57,
    /// The group ID for a dependency
    #[serde(rename = "DEPENDENCY_GROUP_ID")]
    DependencyGroupId = 58,
    #[serde(rename = "SYMBOL")]
    Symbol = 100,
    #[serde(rename = "METHOD_SHORT_NAME")]
    MethodShortName = 102,
    #[serde(rename = "PACKAGE_NAME")]
    PackageName = 103,
    #[serde(rename = "CLASS_NAME")]
    ClassName = 104,
    #[serde(rename = "NODE_LABEL")]
    NodeLabel = 105,
    /// The path of the source file this node was generated from, relative to the root
    /// path in the meta data node. This field must be set but may be set to the value `<unknown>` to
    /// indicate that no source file can be associated with the node, e.g., because the node represents
    /// an entity known to exist because it is referenced, but for which the file that is is declared in
    /// is unknown.
    #[serde(rename = "FILENAME")]
    Filename = 106,
    /// The field contains the names of the overlays applied to this CPG, in order of their
    /// application. Names are free-form strings, that is, this specification does not
    /// dictate them but rather requires tool producers and consumers to communicate them
    /// between each other.
    #[serde(rename = "OVERLAYS")]
    Overlays = 118,
    /// This property contains a hash value in the form of a string.
    /// Hashes can be used to summarize data, e.g., to summarize the
    /// contents of source files or sub graphs. Such summaries are useful
    /// to determine whether code has already been analyzed in incremental
    /// analysis pipelines. This property is optional to allow its calculation
    /// to be deferred or skipped if the hash is not needed.
    #[serde(rename = "HASH")]
    Hash = 120,
    /// For calls involving named parameters, the `ARGUMENT_NAME` field holds the
    /// name of the parameter initialized by the expression. For all other calls,
    /// this field is unset.
    #[serde(rename = "ARGUMENT_NAME")]
    ArgumentName = 130,
    /// This property denotes a key of a key-value pair.
    #[serde(rename = "KEY")]
    Key = 131,
    #[serde(rename = "CLASS_SHORT_NAME")]
    ClassShortName = 132,
    /// This property holds the fully qualified name of the type that the node is
    /// a type alias of.
    #[serde(rename = "ALIAS_TYPE_FULL_NAME")]
    AliasTypeFullName = 158,
    /// The original name of the (potentially mangled) captured variable
    #[serde(rename = "CLOSURE_ORIGINAL_NAME")]
    ClosureOriginalName = 159,
    /// Specifies whether a parameter is the variadic argument handling parameter of
    /// a variadic method. Only one parameter of a method is allowed to have this
    /// property set to true.
    #[serde(rename = "IS_VARIADIC")]
    IsVariadic = 221,
    /// The path to the root directory of the source/binary this CPG is generated from.
    #[serde(rename = "ROOT")]
    Root = 1199,
    /// Type hint for the dynamic type. These are observed to be verifiable at runtime.
    #[serde(rename = "DYNAMIC_TYPE_HINT_FULL_NAME")]
    DynamicTypeHintFullName = 1591,
    /// Similar to `DYNAMIC_TYPE_HINT_FULL_NAME`, but that this makes no guarantee that types within this property are correct. This property is used to capture observations between node interactions during a 'may-analysis'.
    #[serde(rename = "POSSIBLE_TYPES")]
    PossibleTypes = 1592,
    /// Specifies an index, e.g., for a parameter or argument.
    /// Explicit parameters are numbered from 1 to N, while index 0 is reserved for implicit
    /// self / this parameter.
    #[serde(rename = "INDEX")]
    Index = 2223,
    /// This field is experimental. It will likely be removed in the future without any notice.
    /// It stores type information for generic types and methods as well as type information
    /// for members and locals where the type either contains a type parameter reference or
    /// an instantiated type reference.
    #[serde(rename = "GENERIC_SIGNATURE")]
    GenericSignature = 3000,
    /// Start offset into the CONTENT property of the corresponding FILE node.
    /// The offset is such that parts of the content can easily
    /// be accessed via `content.substring(offset, offsetEnd)`.
    /// This means that the offset must be measured in utf16 encoding (i.e. neither in
    /// characters/codeunits nor in byte-offsets into a utf8 encoding).
    /// E.g. for METHOD nodes this start offset points to the start of the methods
    /// source code in the string holding the source code of the entire file.
    #[serde(rename = "OFFSET")]
    Offset = 3812,
    /// End offset (exclusive) into the CONTENT property of the corresponding FILE node.
    /// See OFFSET documentation for finer details.
    /// E.g. for METHOD nodes this end offset points to the first code position which is
    /// not part of the method.
    #[serde(rename = "OFFSET_END")]
    OffsetEnd = 3813,
    /// This field holds the canonical name of a `FIELD_IDENTIFIER`. It is typically
    /// identical to the CODE field, but canonicalized according to source language
    /// semantics. Human readable names are preferable. `FIELD_IDENTIFIER` nodes must
    /// share identical `CANONICAL_NAME` if and
    /// only if they alias, e.g., in C-style unions (if the aliasing relationship is
    /// unknown or there are partial overlaps, then one must make a reasonable guess,
    /// and trade off between false negatives and false positives).
    #[serde(rename = "CANONICAL_NAME")]
    CanonicalName = 2001092,
    /// References to other nodes. This is not a real property; it exists here for the sake of serialization only. valueType and cardinality are meaningless.
    #[serde(rename = "CONTAINED_REF")]
    ContainedRef = 2007161,
}

#[derive(Serialize, Deserialize, Clone, Copy, Debug, PartialEq, Eq, Hash, PartialOrd, Ord)]
pub enum EdgePropertyName {
    #[serde(rename = "UNKNOWN_EDGE_PROPERTY")]
    UnknownEdgeProperty = 0,
    /// This edge property represents the variable propagated by a reaching definition edge.
    #[serde(rename = "VARIABLE")]
    Variable = 11,
}

#[derive(Serialize, Deserialize, Clone, Copy, Debug, PartialEq, Eq, Hash, PartialOrd, Ord)]
pub enum ModifierTypes {
    #[serde(rename = "UNKNOWN_MODIFIER_TYPE")]
    UnknownModifierType = 0,
    /// The static modifier
    #[serde(rename = "STATIC")]
    Static = 1,
    /// The public modifier
    #[serde(rename = "PUBLIC")]
    Public = 2,
    /// The protected modifier
    #[serde(rename = "PROTECTED")]
    Protected = 3,
    /// The private modifier
    #[serde(rename = "PRIVATE")]
    Private = 4,
    /// The abstract modifier
    #[serde(rename = "ABSTRACT")]
    Abstract = 5,
    /// The native modifier
    #[serde(rename = "NATIVE")]
    Native = 6,
    /// The constructor modifier
    #[serde(rename = "CONSTRUCTOR")]
    Constructor = 7,
    /// The virtual modifier
    #[serde(rename = "VIRTUAL")]
    Virtual = 8,
    /// The internal modifier
    #[serde(rename = "INTERNAL")]
    Internal = 9,
    /// The final modifier
    #[serde(rename = "FINAL")]
    Final = 10,
    /// The readonly modifier
    #[serde(rename = "READONLY")]
    Readonly = 11,
    /// Indicate that a method defines a module in the sense e.g. a python module does with the creation of a module object
    #[serde(rename = "MODULE")]
    Module = 12,
    /// Indicate that a method is an anonymous function, lambda, or closure
    #[serde(rename = "LAMBDA")]
    Lambda = 13,
}

#[derive(Serialize, Deserialize, Clone, Copy, Debug, PartialEq, Eq, Hash, PartialOrd, Ord)]
pub enum Languages {
    #[serde(rename = "UNKNOWN_LANGUAGE")]
    UnknownLanguage = 0,
    #[serde(rename = "JAVA")]
    Java = 1,
    #[serde(rename = "JAVASCRIPT")]
    Javascript = 2,
    #[serde(rename = "GOLANG")]
    Golang = 3,
    #[serde(rename = "CSHARP")]
    Csharp = 4,
    #[serde(rename = "C")]
    C = 5,
    #[serde(rename = "PYTHON")]
    Python = 6,
    #[serde(rename = "LLVM")]
    Llvm = 7,
    #[serde(rename = "PHP")]
    Php = 8,
    #[serde(rename = "FUZZY_TEST_LANG")]
    FuzzyTestLang = 9,
    /// generic reverse engineering framework
    #[serde(rename = "GHIDRA")]
    Ghidra = 10,
    #[serde(rename = "KOTLIN")]
    Kotlin = 11,
    /// Eclipse CDT based parser for C/C++
    #[serde(rename = "NEWC")]
    Newc = 12,
    /// Source-based front-end for Java
    #[serde(rename = "JAVASRC")]
    Javasrc = 13,
    /// Source-based front-end for Python
    #[serde(rename = "PYTHONSRC")]
    Pythonsrc = 14,
    /// Source-based JS frontend based on Babel
    #[serde(rename = "JSSRC")]
    Jssrc = 15,
    /// Source-based frontend for Ruby
    #[serde(rename = "RUBYSRC")]
    Rubysrc = 17,
    /// Source-based frontend for Swift
    #[serde(rename = "SWIFTSRC")]
    Swiftsrc = 18,
    /// Source-based frontend for C# and .NET
    #[serde(rename = "CSHARPSRC")]
    Csharpsrc = 19,
}

#[derive(Serialize, Deserialize, Clone, Copy, Debug, PartialEq, Eq, Hash, PartialOrd, Ord)]
pub enum EvaluationStrategies {
    #[serde(rename = "UNKNOWN_EVALUATION_STRATEGY")]
    UnknownEvaluationStrategy = 0,
    /// A parameter or return of a function is passed by reference which means an address is used behind the scenes
    #[serde(rename = "BY_REFERENCE")]
    ByReference = 1,
    /// Only applicable to object parameter or return values. The pointer to the object is passed by value but the object itself is not copied and changes to it are thus propagated out of the method context
    #[serde(rename = "BY_SHARING")]
    BySharing = 2,
    /// A parameter or return of a function passed by value which means a flat copy is used
    #[serde(rename = "BY_VALUE")]
    ByValue = 3,
}

#[derive(Serialize, Deserialize, Clone, Copy, Debug, PartialEq, Eq, Hash, PartialOrd, Ord)]
pub enum DispatchTypes {
    #[serde(rename = "UNKNOWN_DISPATCH_TYPE")]
    UnknownDispatchType = 0,
    /// For statically dispatched calls the call target is known before program execution
    #[serde(rename = "STATIC_DISPATCH")]
    StaticDispatch = 1,
    /// For dynamically dispatched calls the target is determined during runtime
    #[serde(rename = "DYNAMIC_DISPATCH")]
    DynamicDispatch = 2,
    /// For macro expansions, code is inlined.
    #[serde(rename = "INLINED")]
    Inlined = 3,
}

#[derive(Serialize, Deserialize, Clone, Copy, Debug, PartialEq, Eq, Hash, PartialOrd, Ord)]
pub enum ControlStructureTypes {
    #[serde(rename = "UNKNOWN_CONTROL_STRUCTURE_TYPE")]
    UnknownControlStructureType = 0,
    /// Represents a break statement. Labeled breaks are expected to have a JUMP_LABEL
    /// node AST child with ORDER 1
    #[serde(rename = "BREAK")]
    Break = 1,
    /// Represents a continue statement. Labeled continues are expected to have a JUMP_LABEL
    /// node AST child with ORDER 1
    #[serde(rename = "CONTINUE")]
    Continue = 2,
    /// Represents a while statement
    #[serde(rename = "WHILE")]
    While = 3,
    /// Represents a do statement
    #[serde(rename = "DO")]
    Do = 4,
    /// Represents a for statement
    #[serde(rename = "FOR")]
    For = 5,
    /// Represents a goto statement
    #[serde(rename = "GOTO")]
    Goto = 6,
    /// Represents an if statement
    #[serde(rename = "IF")]
    If = 7,
    /// Represents an else statement
    #[serde(rename = "ELSE")]
    Else = 8,
    /// Represents a switch statement
    #[serde(rename = "SWITCH")]
    Switch = 9,
    /// Represents a try statement
    #[serde(rename = "TRY")]
    Try = 10,
    /// Represents a throw statement
    #[serde(rename = "THROW")]
    Throw = 11,
    /// Represents a match expression
    #[serde(rename = "MATCH")]
    Match = 12,
    /// Represents a yield expression
    #[serde(rename = "YIELD")]
    Yield = 13,
    /// Represents a catch clause
    #[serde(rename = "CATCH")]
    Catch = 14,
    /// Represents a finally clause
    #[serde(rename = "FINALLY")]
    Finally = 15,
}


#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_property_value_serialization() {
        // Test string value
        let string_value = PropertyValue {
            value: Some(PropertyValueEnum::StringValue("test string".to_string())),
        };
        let serialized = serde_json::to_string(&string_value).unwrap();
        let expected = r#"{"type":"string_value","value":"test string"}"#;
        assert_eq!(serialized, expected);
        
        // Test deserialization
        let deserialized: PropertyValue = serde_json::from_str(expected).unwrap();
        assert_eq!(deserialized, string_value);
    }

    #[test]
    fn test_property_value_bool() {
        let bool_value = PropertyValue {
            value: Some(PropertyValueEnum::BoolValue(true)),
        };
        let serialized = serde_json::to_string(&bool_value).unwrap();
        let expected = r#"{"type":"bool_value","value":true}"#;
        assert_eq!(serialized, expected);
        
        let deserialized: PropertyValue = serde_json::from_str(expected).unwrap();
        assert_eq!(deserialized, bool_value);
    }

    #[test]
    fn test_property_value_int() {
        let int_value = PropertyValue {
            value: Some(PropertyValueEnum::IntValue(42)),
        };
        let serialized = serde_json::to_string(&int_value).unwrap();
        let expected = r#"{"type":"int_value","value":42}"#;
        assert_eq!(serialized, expected);
        
        let deserialized: PropertyValue = serde_json::from_str(expected).unwrap();
        assert_eq!(deserialized, int_value);
    }

    #[test]
    fn test_property_value_long() {
        let long_value = PropertyValue {
            value: Some(PropertyValueEnum::LongValue(9223372036854775807)),
        };
        let serialized = serde_json::to_string(&long_value).unwrap();
        let expected = r#"{"type":"long_value","value":9223372036854775807}"#;
        assert_eq!(serialized, expected);
        
        let deserialized: PropertyValue = serde_json::from_str(expected).unwrap();
        assert_eq!(deserialized, long_value);
    }

    #[test]
    fn test_property_value_float() {
        let float_value = PropertyValue {
            value: Some(PropertyValueEnum::FloatValue(3.14)),
        };
        let serialized = serde_json::to_string(&float_value).unwrap();
        let expected = r#"{"type":"float_value","value":3.14}"#;
        assert_eq!(serialized, expected);
        
        let deserialized: PropertyValue = serde_json::from_str(expected).unwrap();
        assert_eq!(deserialized, float_value);
    }

    #[test]
    fn test_property_value_double() {
        let double_value = PropertyValue {
            value: Some(PropertyValueEnum::DoubleValue(2.71828)),
        };
        let serialized = serde_json::to_string(&double_value).unwrap();
        let expected = r#"{"type":"double_value","value":2.71828}"#;
        assert_eq!(serialized, expected);
        
        let deserialized: PropertyValue = serde_json::from_str(expected).unwrap();
        assert_eq!(deserialized, double_value);
    }

    #[test]
    fn test_property_value_string_list() {
        let string_list = PropertyValue {
            value: Some(PropertyValueEnum::StringList(StringList {
                values: vec!["one".to_string(), "two".to_string(), "three".to_string()],
            })),
        };
        let serialized = serde_json::to_string(&string_list).unwrap();
        let expected = r#"{"type":"string_list","value":{"values":["one","two","three"]}}"#;
        assert_eq!(serialized, expected);
        
        let deserialized: PropertyValue = serde_json::from_str(expected).unwrap();
        assert_eq!(deserialized, string_list);
    }

    #[test]
    fn test_property_value_contained_refs() {
        let contained_refs = PropertyValue {
            value: Some(PropertyValueEnum::ContainedRefs(ContainedRefs {
                local_name: "test_refs".to_string(),
                refs: vec![1, 2, 3, 4],
            })),
        };
        let serialized = serde_json::to_string(&contained_refs).unwrap();
        let expected = r#"{"type":"contained_refs","value":{"local_name":"test_refs","refs":[1,2,3,4]}}"#;
        assert_eq!(serialized, expected);
        
        let deserialized: PropertyValue = serde_json::from_str(expected).unwrap();
        assert_eq!(deserialized, contained_refs);
    }

    #[test]
    fn test_node_serialization() {
        let node = Node {
            key: 123,
            r#type: NodeType::Method,
            property: vec![
                NodeProperty {
                    name: NodePropertyName::Name,
                    value: Some(PropertyValue {
                        value: Some(PropertyValueEnum::StringValue("testMethod".to_string())),
                    }),
                },
                NodeProperty {
                    name: NodePropertyName::FullName,
                    value: Some(PropertyValue {
                        value: Some(PropertyValueEnum::StringValue("com.example.TestClass.testMethod".to_string())),
                    }),
                },
            ],
        };
        
        let serialized = serde_json::to_string(&node).unwrap();
        let deserialized: Node = serde_json::from_str(&serialized).unwrap();
        assert_eq!(deserialized, node);
    }

    #[test]
    fn test_edge_serialization() {
        let edge = Edge {
            src: 123,
            dst: 456,
            r#type: EdgeType::Ast,
            property: vec![
                EdgeProperty {
                    name: EdgePropertyName::Variable,
                    value: Some(PropertyValue {
                        value: Some(PropertyValueEnum::StringValue("testVar".to_string())),
                    }),
                },
            ],
        };
        
        let serialized = serde_json::to_string(&edge).unwrap();
        let deserialized: Edge = serde_json::from_str(&serialized).unwrap();
        assert_eq!(deserialized, edge);
    }

    #[test]
    fn test_cpg_struct_serialization() {
        let cpg = Cpg {
            node: vec![
                Node {
                    key: 1,
                    r#type: NodeType::Method,
                    property: vec![
                        NodeProperty {
                            name: NodePropertyName::Name,
                            value: Some(PropertyValue {
                                value: Some(PropertyValueEnum::StringValue("main".to_string())),
                            }),
                        },
                    ],
                },
                Node {
                    key: 2,
                    r#type: NodeType::MethodReturn,
                    property: vec![],
                },
            ],
            edge: vec![
                Edge {
                    src: 1,
                    dst: 2,
                    r#type: EdgeType::Ast,
                    property: vec![],
                },
            ],
        };
        
        let serialized = serde_json::to_string(&cpg).unwrap();
        let deserialized: Cpg = serde_json::from_str(&serialized).unwrap();
        assert_eq!(deserialized, cpg);
    }

    #[test]
    fn test_diff_graph_serialization() {
        let diff_graph = DiffGraph {
            entries: vec![
                DiffGraphEntry::Node(Node {
                    key: 1,
                    r#type: NodeType::Method,
                    property: vec![],
                }),
                DiffGraphEntry::Edge(Edge {
                    src: 1,
                    dst: 2,
                    r#type: EdgeType::Ast,
                    property: vec![],
                }),
                DiffGraphEntry::RemoveNode(RemoveNode {
                    key: 3,
                }),
            ],
        };
        
        let serialized = serde_json::to_string(&diff_graph).unwrap();
        let deserialized: DiffGraph = serde_json::from_str(&serialized).unwrap();
        assert_eq!(deserialized, diff_graph);
    }

    #[test]
    fn test_node_type_serialization() {
        let node_type = NodeType::Method;
        let serialized = serde_json::to_string(&node_type).unwrap();
        assert_eq!(serialized, r#""METHOD""#);
        
        let deserialized: NodeType = serde_json::from_str(&serialized).unwrap();
        assert_eq!(deserialized, NodeType::Method);
    }

    #[test]
    fn test_edge_type_serialization() {
        let edge_type = EdgeType::Call;
        let serialized = serde_json::to_string(&edge_type).unwrap();
        assert_eq!(serialized, r#""CALL""#);
        
        let deserialized: EdgeType = serde_json::from_str(&serialized).unwrap();
        assert_eq!(deserialized, EdgeType::Call);
    }

    #[test]
    fn test_property_value_none() {
        let none_value = PropertyValue {
            value: None,
        };
        let serialized = serde_json::to_string(&none_value).unwrap();
        let expected = r#"{}"#;
        assert_eq!(serialized, expected);
        
        let deserialized: PropertyValue = serde_json::from_str(expected).unwrap();
        assert_eq!(deserialized, none_value);
    }
}
