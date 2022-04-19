
read_externalinput <- function(){

externalinput <- list()

########################################################
# Special dates
########################################################
externalinput$StartDate <- dmy("03.03.2020")
externalinput$EndDate <- dmy("18.12.2021")
externalinput$StartDateKP <- dmy("25.05.2020")
externalinput$EndDateKP <- dmy("18.12.2021")

########################################################
# Verified wrong IDs
########################################################
externalinput$wrong_id = "ID000977"
externalinput$wrong_id2 = "#NV"

########################################################
# Quarantine duration
########################################################
Q_Duration <- bind_rows(
  tibble(
    dates = seq(externalinput$StartDate, dmy("30.11.2020"), by=1),
    Q_Duration = "Q_Duration_1",
    Q_Duration_value = "Gesundheitsüberwachung bis zum 14. Tag nach dem letzten Kontakt mit dem bestätigten 2019-nCoV-Fall [...] Kontaktreduktion nach Maßgabe des Gesundheitsamtes. Dies kann, gemäß § 31 IfSG, die Isolation im Krankenhaus beinhalten, oder eine Selbstisolierung während der weiteren diagnostischen Abklärung unter Einhaltung infektionshygienischer Maßnahmen.",
    Q_Duration_shortvalue = "14 days",
    Q_Duration_url = "https://web.archive.org/web/20200212092830/https://www.rki.de/DE/Content/InfAZ/N/Neuartiges_Coronavirus/Kontaktperson/Management.html"
  ),
  tibble(
    dates = seq(dmy("01.12.2020"),dmy("15.02.2021"), by=1),
    Q_Duration = "Q_Duration_2",
    Q_Duration_value = "Häusliche Absonderung für 14 Tage (Quarantäne) - gerechnet ab dem letzten Tag des Kontaktes zum Quellfall. Die häusliche Absonderung kann auf 10 Tage verkürzt werden, wenn ein negativer SARS-CoV-2-Test vorliegt; der Test darf frühestens am zehnten Tag der Quarantäne durchgeführt werden.",
    Q_Duration_shortvalue = "14 days - out with PoC-Test after 10 days",
    Q_Duration_url = "https://web.archive.org/web/20201203010328/https://www.rki.de/DE/Content/InfAZ/N/Neuartiges_Coronavirus/Kontaktperson/Management.html",
  ),
  tibble(
    dates = seq(dmy("16.02.2021"),dmy("08.09.2021"), by=1),
    Q_Duration = "Q_Duration_3",
    Q_Duration_value = "Kontaktpersonen der Kategorie 1 müssen sich unverzüglich für 14 Tage häuslich absondern (Quarantäne) - gerechnet ab dem letzten Tag des Kontaktes zum Quellfall.",
    Q_Duration_shortvalue = "14 Tage",
    Q_Duration_url = "https://web.archive.org/web/20210217232253/https://www.rki.de/DE/Content/InfAZ/N/Neuartiges_Coronavirus/Kontaktperson/Management.html"
  ),
  tibble(
    dates = seq(dmy("09.09.2021"),externalinput$EndDate+14, by=1), 
    Q_Duration = "Q_Duration_4",
    Q_Duration_value = "10 Tage Quarantäne ohne abschließenden Test. 5 Tage mit PCR-Test bei Probenentnahme frühestens am 5. Tag. Bei Personen, die regelmäßig im Rahmen einer seriellen Teststrategie getestet werden (z.B. Schülerinnen und Schüler), kann der negative Nachweis auch mittels qualitativ hochwertigen Antigen-Schnelltests erwogen werden. 7 Tage mit Antigen-Schnelltest bei Probenentnahme frühestens am 7. Tag.",
    Q_Duration_shortvalue = "10 Tage Quarantäne ohne abschließenden Test, 5 Tage mit PCR-Test bei Probenentnahme frühestens am 5. Tag. 7 Tage mit Antigen-Schnelltest",
    Q_Duration_url = "https://web.archive.org/web/20210909213747/https://www.rki.de/DE/Content/InfAZ/N/Neuartiges_Coronavirus/Kontaktperson/Management.html"
  )
)

########################################################
# Isolation duration
########################################################
I_Duration <- bind_rows(
  tibble(
    dates = seq(externalinput$StartDate,dmy("01.07.2020"), by=1), 
    I_Duration = "I_Duration_1",
    I_Duration_value = "Kriterien zur Entlassung aus der häuslichen Isolierung: Frühestens 14 Tage nach Symptombeginn UND Symptomfreiheit seit mind. 48 Stunden bezogen auf die akute COVID-19-Erkrankung", 
    I_Duration_shortvalue  = "14 days", 
    I_Duration_url = "https://web.archive.org/web/20200623101259/https://www.rki.de/DE/Content/InfAZ/N/Neuartiges_Coronavirus/Entlassmanagement.html"
  ),
  tibble(
    dates = seq(dmy("02.07.2020"),dmy("30.03.2021"), by=1) ,
    I_Duration = "I_Duration_2",
    I_Duration_value = "Mind. 48 Stunden Symptomfreiheit PLUS Frühestens 10 Tage nach Symptombeginn",
    I_Duration_shortvalue  = "10 Tage Isolierung",
    I_Duration_url = "https://web.archive.org/web/20200715191510/https://www.rki.de/DE/Content/InfAZ/N/Neuartiges_Coronavirus/Entlassmanagement.html"
  ),
  tibble(
    dates = seq(dmy("31.03.2021"),externalinput$EndDate+14, by=1)  ,
    I_Duration = "I_Duration_3",
    I_Duration_value = "Für Patienten mit leichtem oder mildem/ moderatem Krankheitsverlauf (gemäß WHO-Definition) und ungestörter Immunkompetenz kann eine Entisolierung erfolgen, wenn (1) mindestens 14 Tage seit Auftreten der ersten Symptome verstrichen sind, (2) eine nachhaltige Besserung der akuten COVID-19-Symptomatik gemäß ärztlicher Beurteilung seit >48 h vorliegt und (3) ein negativer Antigentest.",
    I_Duration_shortvalue  = "14 Tage Isolierung",
    I_Duration_url = "https://web.archive.org/web/20210331212322/https://www.rki.de/DE/Content/InfAZ/N/Neuartiges_Coronavirus/Entlassmanagement.html",
  )
)

########################################################
# Contact person defintion
########################################################
Q_Def <- bind_rows(
  tibble(
    dates =  seq(dmy("11.02.2020"),dmy("30.03.2021"), by=1), 
    Q_Def = "Q_Def_1",
    Q_Def_value = "Personen mit kumulativ mindestens 15-minütigem Gesichts- (face-to-face) Kontakt, z.B. im Rahmen eines Gesprächs. Dazu gehören z.B. Personen aus Lebensgemeinschaften im selben Haushalt. Personen mit direktem Kontakt zu Sekreten oder Körperflüssigkeiten, insbesondere zu respiratorischen Sekreten eines bestätigten COVID-19-Falls, wie z.B. Küssen, Kontakt zu Erbrochenem, Mund-zu-Mund Beatmung, Anhusten, Anniesen, etc. Personen, die nach Risikobewertung durch das Gesundheitsamt mit hoher Wahrscheinlichkeit einer relevanten Konzentration von Aerosolen ausgesetzt waren (z.B. Feiern, gemeinsames Singen oder Sporttreiben in Innenräumen) Medizinisches Personal mit Kontakt zum bestätigten COVID-19-Fall im Rahmen von Pflege oder medizinischer Untersuchung (<= 2m), ohne verwendete Schutzausrüstung. Falls die Person früher als COVID-19 Fall gemeldet wurde ist keine Quarantäne erforderlich, es soll ein Selbstmonitoring erfolgen und bei Auftreten von Symptomen eine sofortige Selbst-Isolation und -Testung. Bei positivem Test wird die Kontaktperson zu einem Fall. Bei diesem sollten alle Maßnahmen ergriffen werden wie bei sonstigen Fällen auch (inkl. Isolation). Kontaktpersonen eines bestätigten COVID-19-Falls im Flugzeug: Passagiere, die direkter Sitznachbar des bestätigten COVID-19-Falls waren, unabhängig von der Flugzeit. Saß der COVID-19-Fall am Gang, so zählt der Passagier in derselben Reihe jenseits des Ganges nicht als Kontaktperson der Kategorie I, sondern als Kontaktperson der Kategorie II.  Besatzungsmitglieder oder andere Passagiere, sofern auf Hinweis des bestätigten COVID-19-Falls eines der anderen Kriterien zutrifft (z.B. längeres Gespräch; o.ä.).",
    Q_Def_shortvalue  = "15 min. Group",
    Q_Def_url  = "https://web.archive.org/web/20200815104700/https://www.rki.de/DE/Content/InfAZ/N/Neuartiges_Coronavirus/Kontaktperson/Management.html"
  ),
  tibble(
    dates = seq(dmy("31.03.2021"),dmy("19.05.2021"), by=1),
    Q_Def = "Q_Def_2",
    Q_Def_value = "Enger Kontakt (<1,5 m, Nahfeld) länger als 10 Minuten ohne adäquaten Schutz# (adäquater Schutz = Fall und Kontaktperson tragen durchgehend und korrekt MNS [Mund-Nasen-Schutz] oder FFP2-Maske). Gespräch mit dem Fall (face-to-face-Kontakt, <1,5 m, unabhängig von der Gesprächsdauer) ohne adäquaten Schutz# (adäquater Schutz = Fall und Kontaktperson tragen durchgehend und korrekt MNS [Mund-Nasen-Schutz] oder FFP2-Maske). Gleichzeitiger Aufenthalt von Kontaktperson und Fall im selben Raum mit wahrscheinlich hoher Konzentration infektiöser Aerosole unabhängig vom Abstand für > 10 Minuten, auch wenn durchgehend und korrekt MNS (Mund-Nasen-Schutz) oder FFP2-Maske getragen wurde. Vollständig gegen COVID-19 geimpfte Personen sind nach Exposition zu einem bestätigten SARS-CoV-2-Fall von Quarantäne-Maßnahmen ausgenommen, ebenso wie Personen, die in der Vergangenheit eine PCR-bestätigte und symptomatische COVID-19-Erkrankung durchgemacht haben („Genesene“) und mit einer Impfstoffdosis geimpft sind.",
    Q_Def_shortvalue  = "10 min. + Group + VaccinatedNot",
    Q_Def_url  = "https://web.archive.org/web/20210407214300/https://www.rki.de/DE/Content/InfAZ/N/Neuartiges_Coronavirus/Kontaktperson/Management.html"  
  ),
  tibble(
    dates =  seq(dmy("20.05.2021"), externalinput$EndDate, by=1),
    Q_Def = "Q_Def_3",
    Q_Def_value = "    Enger Kontakt (<1,5 m, Nahfeld) länger als 10 Minuten ohne adäquaten Schutz# (adäquater Schutz = Fall und Kontaktperson tragen durchgehend und korrekt MNS [Mund-Nasen-Schutz] oder FFP2-Maske). Gespräch mit dem Fall (Face-to-face-Kontakt, <1,5 m, unabhängig von der Gesprächsdauer) ohne adäquaten Schutz# (adäquater Schutz = Fall und Kontaktperson tragen durchgehend und korrekt MNS [Mund-Nasen-Schutz] oder FFP2-Maske) oder direkter Kontakt (mit respiratorischem Sekret). Gleichzeitiger Aufenthalt von Kontaktperson und Fall im selben Raum mit wahrscheinlich hoher Konzentration infektiöser Aerosole unabhängig vom Abstand für > 10 Minuten, auch wenn durchgehend und korrekt MNS (Mund-Nasen-Schutz) oder FFP2-Maske getragen wurde. \n Coronavirus-Update (Internes Dokument des Gesundheitsamtes Reinickendorf): Umgang mit Kontaktpersonen in Schulen Wenn wir einen Fall in einer Schulklasse hatten, wird die gesamte Klasse über eine Fernfeldübertragung nur noch bei den folgenden Konstellationen in Quarantäne gesteckt: 1) Es sind schon Übertragungen in der Klasse in der zu bewertenden Situation bekannt geworden 2) Es wurde gar nicht gelüftet und die Zeit zusammen im Raum war über 2h. 3) Sonderfälle, bei dem grob viele Hygieneregeln missachtet wurden, nach Rücksprache. Davon unbenommen bleibt aber, dass die Personen durch eine Nahfeldübertragung Kontaktpersonen geworden sind. Zum Beispiel die besten Kumpels in der Klasse, die eng sitzenden Sitznachbarn, die Erziehungskraft, die das Kind ganz eng betreut hat.",
    Q_Def_shortvalue  = "10 min. + Indidividual children + Unvaccinated dont have to go",
    Q_Def_url  = "Corona-Update",
  )
)

########################################################
# Join the time periods
########################################################
externalinput$zeiten <- tibble(dates =seq(externalinput$StartDate, externalinput$EndDate, by = 1)) %>% 
  left_join(Q_Def, by = "dates") %>% 
  left_join(I_Duration, by = "dates") %>% 
  left_join(Q_Duration, by = "dates")

externalinput
}

########################################################
# Create a check function for testing purposes
########################################################
checkanonids <- function(){
  # df %>% count(AnonID) %>% filter(n>3) %>% pull(AnonID)
  print(df %>% filter(AnonID=="ID021580"))
  print(df %>% filter(AnonID == "ID037188"))
  print(df %>% filter(AnonID == "ID015472"))
}