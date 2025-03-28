# Installation Instructions

This document provides instructions on how to install and run our Settlers of Catan board generator project.

## Prerequisites

Before you begin, ensure you have the following installed on your system:

- OCaml (version 4.14.0 or later recommended)
- OPAM (OCaml's package manager)
- Dune (build system for OCaml)

## Installation Steps

1. **Clone the repository**

   ```bash
   git clone <repository-url>
   cd 3110_final_project
   ```

2. **Install dependencies using OPAM**

   Our project uses standard OCaml libraries, so no additional packages need to be installed. However, you should ensure your OCaml environment is up to date:

   ```bash
   opam update
   opam upgrade
   ```

## Building the Project

To build the project, run:

```bash
dune build
```

## Running the Project

To run the Catan board generator, use:

```bash
dune exec bin/main.exe
```

This will display a hexagonal Catan board with resources and numbers assigned to each tile.

## Running Tests

To run the test suite (once implemented):

```bash
dune test
```
