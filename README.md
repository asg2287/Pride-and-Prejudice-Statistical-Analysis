# Multi-Dimensional Statistical Analysis of Jane Austen's *Pride and Prejudice*

Developed by **Alexander Georgiev** 
*Undergraduate Research Fellow, Columbia University*

This repository houses the end-to-end computational pipeline and R implementation scripts used to model Jane Austen's *Pride and Prejudice* as a closed, signal-processing system. By translating textual attributes into multi-dimensional coordinates, this project analyzes hidden narrative specifics, character hierarchies, and formal constraints governing the novel's structure.

---

## 🛠️ Repository Architecture & Execution Order

The codebase is organized into modular directories reflecting the chronological stages of the quantitative research pipeline. To replicate the study's empirical metrics and diagnostic validations, scripts inside these folders should be reviewed or executed in the following strict sequential order:

```text
📁 pride-and-prejudice-analysis/
│
├── 📁 clustering/                  # Step 1: Character Hierarchy, Clustering and Subclustering
│   └── [Scripts]                  
│
├── 📁 testing/                     # Step 2: Multivariate Diagnostic Testing
│   └── [Scripts]                   
│
├── 📁 intensification/             # Step 3: Intensification Analysis Across the 6 Components
│   └── [Scripts]               
│
├── 📁 joint_distributions/          # Step 4: Structural Cross-Correlations
│   └── [Scripts]                  
│
├── 📁 rhythms/                     # Step 5: Narrative Rhythms of Presence and Absence by K-Means Cluster
│   └── [Scripts]                 
│
├── 📁 riemann/                     # Step 6: Cumulative Geometric Mass and Definite Riemann Integration
│   └── [Scripts]                  
│
└── 📁 percentage_data/             # Step 7: Relative Architectural Composition and Percentage-Based Analysis
    └── [Scripts]                  
