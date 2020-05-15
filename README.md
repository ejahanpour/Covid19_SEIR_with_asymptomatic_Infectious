---
title: "Stochastic SEIR model for Covid-19"
output: html_document
---

## SEIR Model

There are multiple epedimiological models (such as SIR, SEIR, SIRS) developed to simulate and project disease transmission within a community or graph. Based on the disease attributes different models can be more useful. The SEIR models the flows of people between four states: susceptible (S), exposed (E), infected (I), and resistant (R) in a given community. In order to model how disease is transmitting in a community (city, county, ...), SEIR gets in different clinical and community-related features and simulate how individuals within the community changes their status from being Susceptible to Exposed to Infected to Resistant. Below are the list of features we have considered to model how Covid-19 impacts the people in a society. We categorized those into clinical and socio-economic features. The image below visualizes the phase transmission in a community and it shows how those parameters are implemented in the model

![](images/SEIR.jpg)

### Clinical Features:

- **Incubation period**: time between being exposed to a disease and when the symptoms start
- **Asymptomatic infectious rate ($\alpha$)**: percentages of the cases where individual does not develop symptoms.
- **Mild symptomatic rates (1 - $\alpha$)**: percentages of the cases where individual develop mild symptoms.
- **Severe symptomatic rates ($\gamma_1$)**: percentages of the cases where individual hospitalized.
- **Critical symptomatic rates ($\gamma_2$)**: percentages of the cases where individual require ICU
- **Mortality rate ($\mu$)**: percentages of the cases who are deceased.
- **Asymptomatic to recover time**: the time it takes for individuals to recover and become resistant. We assume that recovered cases do not become susceptible again. Also we assume the infected individuals are contagious during this period.
- **Mild to recovery time**: time it takes for individuals to recover after developing symptoms
- **Mild to severe time**: time it takes for individual to hospitalized after developing symptoms
- **Severe to Critical time**: time it takes for hospitalized case to require ICU beds and ventilators

### Community Features:
- **Contact rate between asymptomatic and susceptible ($\beta_0$)**: Daily contact rate between an asymptomatic individual with the rest of the community
- **Contact rate between mild cases and susceptible ($\beta_1$)**: Daily contact rate between an mildly infected individual with the rest of the community
- **Contact rate between severe cases and susceptible ($\beta_2$)**: Daily contact rate between a hospitalized individual with the rest of the community
- **Contact rate between critical cases and susceptible ($\beta_3$)**: Daily contact rate between a severely infected individual with the rest of the community
- **Community population**
- **initial confirmed day**: the date when first case is confirmed
- **Initial confirmed number**: number of confirmed individuals on first day


