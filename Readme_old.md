# Script for COVID-19 isolation and quarantine orders in the district Berlin-Reinickendorf (Germany): How many, how long and to whom?

## Ziel 
Darstellung der Anzahl an Absonderungen (Isolationen und Quarantänen)

## Mitarbeitende
Lisa Kühne
Sophie Bruessermann
Sonja Jäckle
Benjamin Geisler
Jakob Schumacher

## Entwurf
Der Entwurf ist auf Overleaf. [Hier ist der Link](https://www.overleaf.com/project/619e11b5b053fe675d11fac3)

## Ordnerstruktur
├── data
│   ├── demographiedaten
│   ├── reinickendorfer quarantänedaten
├── R
│   ├── absonderungslast.r
│   ├── demographie.r
├── bibliography
│   ├── zitierte publikationen
├── overleaf
│   ├── downloads of overleaf als Archiv (freie Version)

# Steps to reproduce
Setup: You will only need to run this once, but it will take a few minutes to finish. On Linux see also: https://stackoverflow.com/questions/61204259/how-can-i-resolve-the-no-font-name-issue-when-importing-fonts-into-r-using-ext
- Install https://fonts.google.com/specimen/PT+Sans on your local computer
- install.packages("extrafont")
- library(extrafont)
- font_import() # Import all the .ttf files from your system. 