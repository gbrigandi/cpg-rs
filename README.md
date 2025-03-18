# CPG-RS: Code Property Graph Library for Rust

A Rust library for working with Code Property Graphs (CPG), a powerful representation of source code that enables advanced static analysis and vulnerability detection.

## What are Code Property Graphs?

Code Property Graphs (CPGs) were introduced by Fabian Yamaguchi et al. in their 2014 paper ["Modeling and Discovering Vulnerabilities with Code Property Graphs"](https://www.sec.cs.tu-bs.de/pubs/2014-ieeesp.pdf) presented at the IEEE Symposium on Security and Privacy.

CPGs combine multiple program representations into a single unified graph structure:
- **Abstract Syntax Trees (AST)**: Represent the syntactic structure of code
- **Control Flow Graphs (CFG)**: Represent the flow of control between statements
- **Program Dependence Graphs (PDG)**: Represent data dependencies between program elements

By merging these representations, CPGs enable more sophisticated code analysis than any single representation could provide alone. This unified approach allows for complex queries that can identify security vulnerabilities, bugs, and code quality issues that would be difficult to detect with traditional methods.

## Notable CPG Implementations

Several open-source projects implement the CPG concept:

- [codepropertygraph](https://github.com/ShiftLeftSecurity/codepropertygraph): The original implementation by the authors of the CPG paper.
- [Joern](https://github.com/joernio/joern): Code analysis platform for various languages, based on codepropertygraph. 

This library, cpg-rs, provides a Rust implementation with serialization/deserialization support.

## Features

- Serialization and deserialization of CPG structures using serde
- Support for nodes, edges, and properties
- Support for diff graphs to represent changes to CPGs
- Comprehensive type system for CPG elements

## Examples

### Modifying a CPG

The `modify_cpg` example demonstrates how to:
1. Create a new CPG from scratch
2. Save it to a JSON file
3. Load it back
4. Modify it by adding nodes and edges
5. Save the modified CPG

```bash
cargo run --example modify_cpg
```

### Finding Methods and Parameters

The `find_methods` example shows how to:
1. Load a CPG from a JSON file
2. Find all method nodes and their parameters
3. Print a summary of the methods and parameters

```bash
cargo run --example find_methods
```

### Working with Diff Graphs

The `diff_graph` example demonstrates how to:
1. Create a diff graph representing changes to a CPG
2. Save it to a JSON file
3. Load it back
4. Print a summary of the changes

```bash
cargo run --example diff_graph
```

## Usage

Add this to your `Cargo.toml`:

```toml
[dependencies]
cpg-rs = "0.1.0"
```

Basic usage:

```rust
use cpg_rs::{Cpg, Node, Edge, NodeType, EdgeType};
use std::fs::File;
use std::io::{BufReader, BufWriter};

// Load a CPG
let file = File::open("cpg.json").unwrap();
let reader = BufReader::new(file);
let cpg: Cpg = serde_json::from_reader(reader).unwrap();

// Process the CPG...

// Save the CPG
let file = File::create("modified_cpg.json").unwrap();
let writer = BufWriter::new(file);
serde_json::to_writer_pretty(writer, &cpg).unwrap();
```

## License

Apache License, Version 2.0 ([LICENSE-APACHE](LICENSE-APACHE) or http://www.apache.org/licenses/LICENSE-2.0)

## Author

Gianluca Brigandi <gbrigand@gmail.com>
