/*!
 * # Modify CPG Example
 * 
 * This example demonstrates how to create, modify, and save a Code Property Graph (CPG).
 * 
 * ## What this example shows:
 * 
 * - Creating a CPG from scratch with nodes representing:
 *   - Files
 *   - Methods
 *   - Method returns
 * - Connecting nodes with edges to represent relationships
 * - Adding properties to nodes and edges
 * - Modifying an existing CPG by:
 *   - Adding new nodes and edges
 *   - Modifying properties of existing nodes
 *   - Adding complex property types like lists and references
 * - Serializing and deserializing CPGs to/from JSON
 * 
 * This example provides a comprehensive overview of working with CPGs in cpg-rs,
 * showing how to represent program structures and their relationships.
 */

use cpg_rs::{
    ContainedRefs, Cpg, Edge, EdgeProperty, EdgePropertyName, EdgeType, Node, NodeProperty,
    NodePropertyName, NodeType, PropertyValue, PropertyValueEnum, StringList,
};
use std::fs::File;
use std::io::{BufReader, BufWriter};
use std::path::Path;

fn main() -> Result<(), Box<dyn std::error::Error>> {
    let cpg = create_sample_cpg();
    save_cpg(&cpg, "sample_cpg.json")?;
    println!("Created and saved a sample CPG to sample_cpg.json");

    let mut cpg = load_cpg("sample_cpg.json")?;
    modify_cpg(&mut cpg);
    save_cpg(&cpg, "modified_cpg.json")?;
    println!("Modified the CPG and saved it to modified_cpg.json");

    Ok(())
}

fn create_sample_cpg() -> Cpg {
    let file_node = Node {
        key: 1,
        r#type: NodeType::File,
        property: vec![
            NodeProperty {
                name: NodePropertyName::Name,
                value: Some(PropertyValue {
                    value: Some(PropertyValueEnum::StringValue("example.java".to_string())),
                }),
            },
            NodeProperty {
                name: NodePropertyName::Filename,
                value: Some(PropertyValue {
                    value: Some(PropertyValueEnum::StringValue("example.java".to_string())),
                }),
            },
        ],
    };

    let method_node = Node {
        key: 2,
        r#type: NodeType::Method,
        property: vec![
            NodeProperty {
                name: NodePropertyName::Name,
                value: Some(PropertyValue {
                    value: Some(PropertyValueEnum::StringValue("main".to_string())),
                }),
            },
            NodeProperty {
                name: NodePropertyName::FullName,
                value: Some(PropertyValue {
                    value: Some(PropertyValueEnum::StringValue(
                        "com.example.Main.main".to_string(),
                    )),
                }),
            },
            NodeProperty {
                name: NodePropertyName::Signature,
                value: Some(PropertyValue {
                    value: Some(PropertyValueEnum::StringValue(
                        "void main(String[] args)".to_string(),
                    )),
                }),
            },
            NodeProperty {
                name: NodePropertyName::LineNumber,
                value: Some(PropertyValue {
                    value: Some(PropertyValueEnum::IntValue(10)),
                }),
            },
        ],
    };

    let return_node = Node {
        key: 3,
        r#type: NodeType::MethodReturn,
        property: vec![
            NodeProperty {
                name: NodePropertyName::TypeFullName,
                value: Some(PropertyValue {
                    value: Some(PropertyValueEnum::StringValue("void".to_string())),
                }),
            },
            NodeProperty {
                name: NodePropertyName::Code,
                value: Some(PropertyValue {
                    value: Some(PropertyValueEnum::StringValue("RET".to_string())),
                }),
            },
        ],
    };

    // Create edges
    let file_to_method_edge = Edge {
        src: 1, // file node
        dst: 2, // method node
        r#type: EdgeType::Contains,
        property: vec![],
    };

    let method_to_return_edge = Edge {
        src: 2, // method node
        dst: 3, // return node
        r#type: EdgeType::Ast,
        property: vec![],
    };

    Cpg {
        node: vec![file_node, method_node, return_node],
        edge: vec![file_to_method_edge, method_to_return_edge],
    }
}

fn load_cpg<P: AsRef<Path>>(path: P) -> Result<Cpg, Box<dyn std::error::Error>> {
    let file = File::open(path)?;
    let reader = BufReader::new(file);
    let cpg = serde_json::from_reader(reader)?;
    Ok(cpg)
}

fn save_cpg<P: AsRef<Path>>(cpg: &Cpg, path: P) -> Result<(), Box<dyn std::error::Error>> {
    let file = File::create(path)?;
    let writer = BufWriter::new(file);
    serde_json::to_writer_pretty(writer, cpg)?;
    Ok(())
}

fn modify_cpg(cpg: &mut Cpg) {
    let param_node = Node {
        key: 4,
        r#type: NodeType::MethodParameterIn,
        property: vec![
            NodeProperty {
                name: NodePropertyName::Name,
                value: Some(PropertyValue {
                    value: Some(PropertyValueEnum::StringValue("args".to_string())),
                }),
            },
            NodeProperty {
                name: NodePropertyName::TypeFullName,
                value: Some(PropertyValue {
                    value: Some(PropertyValueEnum::StringValue(
                        "java.lang.String[]".to_string(),
                    )),
                }),
            },
            NodeProperty {
                name: NodePropertyName::EvaluationStrategy,
                value: Some(PropertyValue {
                    value: Some(PropertyValueEnum::StringValue("BY_VALUE".to_string())),
                }),
            },
        ],
    };

    let local_node = Node {
        key: 5,
        r#type: NodeType::Local,
        property: vec![
            NodeProperty {
                name: NodePropertyName::Name,
                value: Some(PropertyValue {
                    value: Some(PropertyValueEnum::StringValue("message".to_string())),
                }),
            },
            NodeProperty {
                name: NodePropertyName::TypeFullName,
                value: Some(PropertyValue {
                    value: Some(PropertyValueEnum::StringValue(
                        "java.lang.String".to_string(),
                    )),
                }),
            },
            NodeProperty {
                name: NodePropertyName::Code,
                value: Some(PropertyValue {
                    value: Some(PropertyValueEnum::StringValue("String message".to_string())),
                }),
            },
        ],
    };

    let literal_node = Node {
        key: 6,
        r#type: NodeType::Literal,
        property: vec![
            NodeProperty {
                name: NodePropertyName::TypeFullName,
                value: Some(PropertyValue {
                    value: Some(PropertyValueEnum::StringValue(
                        "java.lang.String".to_string(),
                    )),
                }),
            },
            NodeProperty {
                name: NodePropertyName::Code,
                value: Some(PropertyValue {
                    value: Some(PropertyValueEnum::StringValue("Hello, World!".to_string())),
                }),
            },
        ],
    };

    let call_node = Node {
        key: 7,
        r#type: NodeType::Call,
        property: vec![
            NodeProperty {
                name: NodePropertyName::Name,
                value: Some(PropertyValue {
                    value: Some(PropertyValueEnum::StringValue("println".to_string())),
                }),
            },
            NodeProperty {
                name: NodePropertyName::MethodFullName,
                value: Some(PropertyValue {
                    value: Some(PropertyValueEnum::StringValue(
                        "java.io.PrintStream.println".to_string(),
                    )),
                }),
            },
            NodeProperty {
                name: NodePropertyName::TypeFullName,
                value: Some(PropertyValue {
                    value: Some(PropertyValueEnum::StringValue("void".to_string())),
                }),
            },
            NodeProperty {
                name: NodePropertyName::Code,
                value: Some(PropertyValue {
                    value: Some(PropertyValueEnum::StringValue(
                        "System.out.println(message)".to_string(),
                    )),
                }),
            },
        ],
    };

    let method_to_param_edge = Edge {
        src: 2, // method node
        dst: 4, // parameter node
        r#type: EdgeType::Ast,
        property: vec![EdgeProperty {
            name: EdgePropertyName::Variable,
            value: Some(PropertyValue {
                value: Some(PropertyValueEnum::StringValue("args".to_string())),
            }),
        }],
    };

    let method_to_local_edge = Edge {
        src: 2, // method node
        dst: 5, // local node
        r#type: EdgeType::Ast,
        property: vec![],
    };

    let local_to_literal_edge = Edge {
        src: 5, // local node
        dst: 6, // literal node
        r#type: EdgeType::Ast,
        property: vec![],
    };

    let method_to_call_edge = Edge {
        src: 2, // method node
        dst: 7, // call node
        r#type: EdgeType::Ast,
        property: vec![],
    };

    if let Some(method_node) = cpg.node.iter_mut().find(|n| n.key == 2) {
        if let Some(line_prop) = method_node
            .property
            .iter_mut()
            .find(|p| p.name == NodePropertyName::LineNumber)
        {
            line_prop.value = Some(PropertyValue {
                value: Some(PropertyValueEnum::IntValue(12)), // Changed from 10 to 12
            });
        }
    }

    if let Some(method_node) = cpg.node.iter_mut().find(|n| n.key == 2) {
        method_node.property.push(NodeProperty {
            name: NodePropertyName::PossibleTypes,
            value: Some(PropertyValue {
                value: Some(PropertyValueEnum::StringList(StringList {
                    values: vec![
                        "java.lang.Object".to_string(),
                        "com.example.Main".to_string(),
                    ],
                })),
            }),
        });
    }

    if let Some(file_node) = cpg.node.iter_mut().find(|n| n.key == 1) {
        file_node.property.push(NodeProperty {
            name: NodePropertyName::ContainedRef,
            value: Some(PropertyValue {
                value: Some(PropertyValueEnum::ContainedRefs(ContainedRefs {
                    local_name: "methods".to_string(),
                    refs: vec![2], // Reference to the method node
                })),
            }),
        });
    }

    cpg.node.push(param_node);
    cpg.node.push(local_node);
    cpg.node.push(literal_node);
    cpg.node.push(call_node);

    cpg.edge.push(method_to_param_edge);
    cpg.edge.push(method_to_local_edge);
    cpg.edge.push(local_to_literal_edge);
    cpg.edge.push(method_to_call_edge);
}

fn print_cpg_summary(cpg: &Cpg) {
    println!("CPG Summary:");
    println!("  Nodes: {}", cpg.node.len());
    println!("  Edges: {}", cpg.edge.len());

    let mut node_type_counts = std::collections::HashMap::new();
    for node in &cpg.node {
        *node_type_counts
            .entry(format!("{:?}", node.r#type))
            .or_insert(0) += 1;
    }

    println!("  Node types:");
    for (node_type, count) in node_type_counts {
        println!("    {}: {}", node_type, count);
    }

    // Count edge types
    let mut edge_type_counts = std::collections::HashMap::new();
    for edge in &cpg.edge {
        *edge_type_counts
            .entry(format!("{:?}", edge.r#type))
            .or_insert(0) += 1;
    }

    println!("  Edge types:");
    for (edge_type, count) in edge_type_counts {
        println!("    {}: {}", edge_type, count);
    }
}
