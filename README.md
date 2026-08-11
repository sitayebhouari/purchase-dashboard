# 📊 Dashboard des Achats

[![R](https://img.shields.io/badge/R-4.0+-blue)](https://www.r-project.org/)
[![Shiny](https://img.shields.io/badge/Shiny-1.7+-green)](https://shiny.rstudio.com/)
[![License](https://img.shields.io/badge/License-MIT-yellow)](LICENSE)

## 📖 Description

Application Shiny professionnelle pour la **gestion et l'analyse des achats** de projets de construction.

---

## ✨ Fonctionnalités

### 🔐 Authentification
- Système de connexion sécurisé
- 3 rôles: **Admin**, **Tester**, **Viewer**
- Demande d'inscription avec approbation par l'Admin

### 📊 Dashboard
- KPIs en temps réel (Total HT, TVA, TTC, Fournisseurs)
- Évolution des achats par mois
- Répartition des achats par projet
- Top 10 fournisseurs

### 📈 Management
- Évolution mensuelle par année
- Comparaison des projets (HT, TVA, TTC)
- Analyse trimestrielle
- Achat moyen par mois

### 📋 Données
- Tableau des achats avec filtres
- Export Excel et PDF
- Statistiques par fournisseur

### ➕ Ajout
- Ajout de nouvelles transactions
- Calcul automatique (HT, TVA, TTC)

### 👥 Utilisateurs (Admin)
- Ajout/Modification/Suppression
- Gestion des rôles
- Journal d'audit

---

## 👥 Rôles et Permissions

| Rôle | Dashboard | Management | Données | Ajout | Rapports | Utilisateurs | Demandes |
|------|-----------|------------|---------|-------|----------|--------------|----------|
| 👑 **Admin** | ✅ | ✅ | ✅ | ✅ | ✅ | ✅ | ✅ |
| 🧪 **Tester** | ✅ | ✅ | ✅ | ✅ | ✅ | ❌ | ❌ |
| 👁️ **Viewer** | ✅ | ✅ | ✅ | ❌ | ✅ | ❌ | ❌ |

---

## 🚀 Installation

### Prérequis
- **R** (version 4.0 ou supérieure)
- **RStudio** (recommandé)

### Étapes

1. **Cloner le dépôt**
   ```bash
   git clone https://github.com/sitayebhouari/purchase-dashboard.git
   cd purchase-dashboard
