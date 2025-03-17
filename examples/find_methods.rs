/*!
 * # Find Methods Example
 * 
 * This example demonstrates how to traverse a Code Property Graph (CPG) to find
 * and analyze method definitions.
 * 
 * ## What this example shows:
 * 
 * - Loading a CPG from a JSON file
 * - Traversing the graph to find all method nodes
 * - Extracting method parameters and their types
 * - Finding method return types
 * - Working with node properties to extract meaningful information
 * 
 * This example is useful for understanding how to query and extract specific
 * information from a CPG, which is a fundamental operation for many static
 * analysis tools.
 */

use cpg_rs::{Cpg, EdgeType, Node, NodePropertyName, NodeType, PropertyValueEnum};
use std::collections::HashMap;
use std::fs::File;
use std::io::BufReader;
use std::path::Path;

fn main() -> Result<(), Box<dyn std::error::Error>> {
    let cpg = load_cpg("sample_cpg.json")?;

    let methods = find_methods_with_parameters(&cpg);

    println!("Found {} methods:", methods.len());
    for (method, params) in methods {
        println!("Method: {}", method);
        if params.is_empty() {
            println!("  No parameters");
        } else {
            println!("  Parameters:");
            for param in params {
                println!("    {}", param);
            }
        }

        if let Some(return_type) = find_method_return_type(&cpg, &method) {
            println!("  Return type: {}", return_type);
        }
    }

    Ok(())
}

fn load_cpg<P: AsRef<Path>>(path: P) -> Result<Cpg, Box<dyn std::error::Error>> {
    let file = File::open(path)?;
    let reader = BufReader::new(file);
    let cpg = serde_json::from_reader(reader)?;
    Ok(cpg)
}

fn find_methods_with_parameters(cpg: &Cpg) -> Vec<(String, Vec<String>)> {
    let node_map: HashMap<i64, &Node> = cpg.node.iter().map(|n| (n.key, n)).collect();

    let mut method_params: HashMap<i64, Vec<i64>> = HashMap::new();

    for edge in &cpg.edge {
        if edge.r#type == EdgeType::Ast {
            if let Some(src_node) = node_map.get(&edge.src) {
                if src_node.r#type == NodeType::Method {
                    if let Some(dst_node) = node_map.get(&edge.dst) {
                        if dst_node.r#type == NodeType::MethodParameterIn {
                            method_params.entry(edge.src).or_default().push(edge.dst);
                        }
                    }
                }
            }
        }
    }

    let mut result = Vec::new();

    for node in &cpg.node {
        if node.r#type == NodeType::Method {
            // Get the method name (prefer FULL_NAME, fall back to NAME)
            let method_name = get_node_property_string(node, NodePropertyName::FullName)
                .unwrap_or_else(|| {
                    get_node_property_string(node, NodePropertyName::Name)
                        .unwrap_or_else(|| format!("Method_{}", node.key))
                });

            let param_ids = method_params.get(&node.key).cloned().unwrap_or_default();

            // Get the parameter names and types
            let mut param_names = Vec::new();
            for param_id in param_ids {
                let param_node = node_map[&param_id];
                let param_name = get_node_property_string(param_node, NodePropertyName::Name)
                    .unwrap_or_else(|| format!("Param_{}", param_id));

                let param_type =
                    get_node_property_string(param_node, NodePropertyName::TypeFullName)
                        .unwrap_or_else(|| "unknown_type".to_string());

                param_names.push(format!("{}: {}", param_name, param_type));
            }

            result.push((method_name, param_names));
        }
    }

    result
}

fn find_method_return_type(cpg: &Cpg, method_name: &str) -> Option<String> {
    let node_map: HashMap<i64, &Node> = cpg.node.iter().map(|n| (n.key, n)).collect();

    // Find the method node with the given name
    let method_node = cpg.node.iter().find(|node| {
        node.r#type == NodeType::Method
            && get_node_property_string(node, NodePropertyName::FullName)
                .map_or(false, |name| name == method_name)
    })?;

    for edge in &cpg.edge {
        if edge.r#type == EdgeType::Ast && edge.src == method_node.key {
            if let Some(dst_node) = node_map.get(&edge.dst) {
                if dst_node.r#type == NodeType::MethodReturn {
                    return get_node_property_string(dst_node, NodePropertyName::TypeFullName);
                }
            }
        }
    }

    None
}

fn get_node_property_string(node: &Node, property_name: NodePropertyName) -> Option<String> {
    for prop in &node.property {
        if prop.name == property_name {
            if let Some(ref value) = prop.value {
                if let Some(ref value_enum) = value.value {
                    if let PropertyValueEnum::StringValue(s) = value_enum {
                        return Some(s.clone());
                    }
                }
            }
        }
    }
    None
}
