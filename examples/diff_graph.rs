/*!
 * # Diff Graph Example
 * 
 * This example demonstrates how to work with diff graphs in cpg-rs.
 * 
 * A diff graph represents incremental changes to a Code Property Graph (CPG),
 * allowing efficient updates without recreating the entire graph. This is
 * particularly useful for incremental code analysis.
 * 
 * ## What this example shows:
 * 
 * - Creating a diff graph with various operations:
 *   - Adding new nodes and edges
 *   - Removing existing nodes and edges
 *   - Adding properties to existing nodes and edges
 * - Serializing a diff graph to JSON
 * - Deserializing a diff graph from JSON
 * - Analyzing the contents of a diff graph
 * 
 * The diff graph format allows for efficient transmission and application of
 * changes to a CPG, making it ideal for incremental analysis pipelines.
 */

use cpg_rs::{
    DiffGraph, DiffGraphEntry, Edge, EdgeProperty, EdgePropertyName, EdgeType, Node, NodeProperty,
    NodePropertyName, NodeType, PropertyValue, PropertyValueEnum, RemoveEdge, RemoveNode,
};
use std::fs::File;
use std::io::{BufReader, BufWriter};
use std::path::Path;

fn main() -> Result<(), Box<dyn std::error::Error>> {
    let diff_graph = create_sample_diff_graph();

    save_diff_graph(&diff_graph, "sample_diff.json")?;
    println!("Created and saved a sample diff graph to sample_diff.json");

    let loaded_diff = load_diff_graph("sample_diff.json")?;
    println!(
        "Loaded diff graph with {} entries",
        loaded_diff.entries.len()
    );

    print_diff_graph_summary(&loaded_diff);

    Ok(())
}

fn create_sample_diff_graph() -> DiffGraph {
    let mut entries = Vec::new();
    let new_method = Node {
        key: 100,
        r#type: NodeType::Method,
        property: vec![
            NodeProperty {
                name: NodePropertyName::Name,
                value: Some(PropertyValue {
                    value: Some(PropertyValueEnum::StringValue("newMethod".to_string())),
                }),
            },
            NodeProperty {
                name: NodePropertyName::FullName,
                value: Some(PropertyValue {
                    value: Some(PropertyValueEnum::StringValue(
                        "com.example.Main.newMethod".to_string(),
                    )),
                }),
            },
        ],
    };
    entries.push(DiffGraphEntry::Node(new_method));

    let new_edge = Edge {
        src: 1,   // Assuming node 1 exists in the original CPG
        dst: 100, // The new method node
        r#type: EdgeType::Contains,
        property: vec![],
    };
    entries.push(DiffGraphEntry::Edge(new_edge));

    let remove_node = RemoveNode { key: 5 };
    entries.push(DiffGraphEntry::RemoveNode(remove_node));

    let remove_edge = RemoveEdge {
        out_node_key: 2,
        in_node_key: 3,
        edge_type: EdgeType::Ast,
        properties_hash: vec![1, 2, 3, 4], // This would normally be a hash of the edge properties
    };
    entries.push(DiffGraphEntry::RemoveEdge(remove_edge));

    let add_node_property = cpg_rs::AdditionalNodeProperty {
        node_id: 2, // Assuming node 2 exists in the original CPG
        property: Some(NodeProperty {
            name: NodePropertyName::Code, // Using Code instead of Comment
            value: Some(PropertyValue {
                value: Some(PropertyValueEnum::StringValue(
                    "This is a new comment".to_string(),
                )),
            }),
        }),
    };
    entries.push(DiffGraphEntry::NodeProperty(add_node_property));

    let add_edge_property = cpg_rs::AdditionalEdgeProperty {
        edge_id: 0, // Not used in the current schema
        out_node_key: 2,
        in_node_key: 3,
        edge_type: EdgeType::Ast,
        property: Some(EdgeProperty {
            name: EdgePropertyName::Variable,
            value: Some(PropertyValue {
                value: Some(PropertyValueEnum::StringValue("newVariable".to_string())),
            }),
        }),
    };
    entries.push(DiffGraphEntry::EdgeProperty(add_edge_property));

    DiffGraph { entries }
}

fn save_diff_graph<P: AsRef<Path>>(
    diff: &DiffGraph,
    path: P,
) -> Result<(), Box<dyn std::error::Error>> {
    let file = File::create(path)?;
    let writer = BufWriter::new(file);
    serde_json::to_writer_pretty(writer, diff)?;
    Ok(())
}

fn load_diff_graph<P: AsRef<Path>>(path: P) -> Result<DiffGraph, Box<dyn std::error::Error>> {
    let file = File::open(path)?;
    let reader = BufReader::new(file);
    let diff = serde_json::from_reader(reader)?;
    Ok(diff)
}

fn print_diff_graph_summary(diff: &DiffGraph) {
    println!("Diff Graph Summary:");
    let mut add_node_count = 0;
    let mut add_edge_count = 0;
    let mut add_node_prop_count = 0;
    let mut add_edge_prop_count = 0;
    let mut remove_node_count = 0;
    let mut remove_edge_count = 0;
    let mut remove_node_prop_count = 0;
    let mut remove_edge_prop_count = 0;

    for entry in &diff.entries {
        match entry {
            DiffGraphEntry::Node(_) => add_node_count += 1,
            DiffGraphEntry::Edge(_) => add_edge_count += 1,
            DiffGraphEntry::NodeProperty(_) => add_node_prop_count += 1,
            DiffGraphEntry::EdgeProperty(_) => add_edge_prop_count += 1,
            DiffGraphEntry::RemoveNode(_) => remove_node_count += 1,
            DiffGraphEntry::RemoveEdge(_) => remove_edge_count += 1,
            DiffGraphEntry::RemoveNodeProperty(_) => remove_node_prop_count += 1,
            DiffGraphEntry::RemoveEdgeProperty(_) => remove_edge_prop_count += 1,
        }
    }

    println!("  Add operations:");
    println!("    Nodes: {}", add_node_count);
    println!("    Edges: {}", add_edge_count);
    println!("    Node properties: {}", add_node_prop_count);
    println!("    Edge properties: {}", add_edge_prop_count);

    println!("  Remove operations:");
    println!("    Nodes: {}", remove_node_count);
    println!("    Edges: {}", remove_edge_count);
    println!("    Node properties: {}", remove_node_prop_count);
    println!("    Edge properties: {}", remove_edge_prop_count);

    if add_node_count > 0 {
        println!("Added nodes:");
        for entry in &diff.entries {
            if let DiffGraphEntry::Node(node) = entry {
                let node_type = format!("{:?}", node.r#type);
                let name = node
                    .property
                    .iter()
                    .find(|p| p.name == NodePropertyName::Name)
                    .and_then(|p| {
                        if let Some(ref value) = p.value {
                            if let Some(PropertyValueEnum::StringValue(ref s)) = value.value {
                                return Some(s.as_str());
                            }
                        }
                        None
                    })
                    .unwrap_or("unnamed");

                println!("  Node {} ({}): {}", node.key, node_type, name);
            }
        }
    }
}
