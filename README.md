# Gestion-des-Risques-Multiples

---

L'objectif de la présent étude est d'évaluer, au moyen d'une CVAR à 99%, le risque de crédit sur un portefeuille composé de deux créances, issues du secteur bancaire, de même notionnel 1000 EUR et de même maturité 4 ans.

La première est une obligation BNP senior de taux de recouvrement de moyenne 60% et de volatilité 15%, et la seconde est une obligation Société Générale junior (ou subordonnée) de taux de recouvrement de moyenne 30% et de volatilité 25%. On suppose qu’il n’y a pas de dépendance entre les taux de recouvrement.

La perte de crédit attendue sur ce portefeuille peut s'écrire sous la forme mathématique suivante :

```math
L= EAD_{BNP} (1 - R_{BNP}) \mathbf{1}_{\tau_{BNP} \leq 4} + EAD_{SG} (1 - R_{SG}) \mathbf{1}_{\tau_{SG} \leq 4}
```

où :
- `EAD_i` est la perte en cas de défaut. Dans notre cas, elle est égale au notionnel ;
- `R_i` est le taux de recouvrement ;
- `τ_i` est le moment de survenu du défaut.

Nous souhaitons retrouver une CVAR à 99% qui s'écrit :

```math
CVAR_{99\%} = \inf\{x, \mathbf{P}(L \leq x) \geq 99\%\}
```

Dans l'expression donnée de la perte, on note la présence de variables aléatoires (le taux de recouvrement et le moment de survenu du défaut). La caractérisation des pertes est donc conditionnée par la connaissance des distributions de ces variables. Nous allons nous évertuer tout le long de ce projet à modéliser cette VaR et, pour ce faire, nous procédons suivant une méthodologie que nous déroulons ci-après :

---
