library(tidyverse)
library(esquisse)
library(lubridate)

####Import####
odata <- read.csv("Temporary datasets/OldFebruar.csv", numerals = c("no.loss"))
odata$date <- ymd(odata$date)

tm <- read.csv("Temporary datasets/coptwitter_theta.csv", numerals = c("no.loss")) %>%
  select(-X) %>%
  rename(tweetid = "identifier",
         anzeige = "wenden_sachverhalt_zuständige_bekannt_gerne_anzeige_kk",
         fahrzeugkontrolle = "fahrer_autofahrer_schwer_fahrzeug_führerschein_leicht_kontrolle",
         festnahme = "festnahme_fest_ihn_haftbefehl_verdacht_männer_staatsanwaltschaft",
         versammlung = "versammlung_verstöße_einhaltung_kontrollen_maskenpflicht_corona_gilt",
         füllwoerter = "müssen_doch_as_richtig_kein_natürlich_einfach",
         schnee = "schnee_straßen_geschwindigkeit_geschwindigkeitskontrollen_gut_kontrollieren_eis",
         betrüger = "betrüger_tipps_falsche_betrug_geld_achtung_vorsicht",
         advent = "jahr_advent_weihnachten_gesund_kreis_allen_bleiben",
         gewalt = "gewalt_thema_frauen_du_fragen_zivilehelden_lka",
         einbruch = "brand_gestohlen_recklinghausen_einbruch_entwendet_euro_einbrecher",
         sperrung = "richtung_frei_sperrung_aufgehoben_verkehr_lkw_umfahren",
         zeugenaufruf = "zeugengesucht_unbekannter_bremen_zeugenaufruf_pm_raub_zeugensuche", 
         fahndung = "vermisst_mithilfe_bild_öffentlichkeitsfahndung_suche_fahndung_foto",
         kp = "d_m_w_fragen_liegt_coronavirus_sachstand" ,
         abk = "kk_ck_wohnungseinbruchsradar_c_rad_zeigt_ffm",
         ledd = "leipzig_dresden_meldungen_le_düren_k_statt",
         dannenroeder = "dannenroederforst_dannenroeder_einsatzkräfte_gebracht_mannheim_heutigen_krankenhaus",
         ms = "pd_live_münster_trier_feuerwehr_ms_nord",
         städte = "bochum_._polizeime_str_witten_herne_dessau",
         präsi = "ots_pressebericht_unterfranken_polsiwi_wirfüreuch_polizeioe_polizeipräsidiums")

tmjoin <- merge(odata, tm, by = "tweetid") %>%
  select(-text)


#join %>% group_by(date) %>% for (i in join[4:23]) {summarise("mittel" = mean(i))}

###Topic nach Datum####
tmdate <- tmjoin %>% 
  group_by(date) %>%
  summarise(manzeige = mean(anzeige), 
            mfahrzeugkontrolle = mean(fahrzeugkontrolle), 
            mfestnahme = mean(festnahme),
            mversammlung = mean(versammlung),
            mfüllwoerter = mean(füllwoerter),
            mschnee = mean(schnee),
            mbetrüger = mean(betrüger),
            madvent = mean(advent),
            mgewalt = mean(gewalt),
            meinbruch = mean(einbruch),
            msperrung = mean(sperrung),
            mzeugenaufruf = mean(zeugenaufruf),
            mfahndung = mean(fahndung),
            mkp = mean(kp),
            mabk = mean(abk),
            mledd = mean(ledd),
            mdannenröder = mean(dannenroeder),
            mms = mean(ms),
            mstädte = mean(städte),
            mpräsi = mean(präsi))

gude <- ggplot(data = tmdate, aes(x=date)) +
  geom_area(aes(y=madvent, fill = "red"), alpha = 0.7) +
  ylab("Mean theta") +
  xlab("Date") +
  theme(legend.position = "bottom") +
  scale_fill_discrete(name= "Topic", labels = "jahr_advent_weihnachten_gesund_kreis_allen_bleiben")
gude
                      
toptime <- ggplot(data = tmdate, aes(x=date)) +
  geom_area(aes(y= mschnee, fill = "blue"), alpha = 0.5) +
  geom_area(aes(y=mversammlung, fill = "red"), alpha = 0.5) +
  geom_area(aes(y=mbetrüger, fill = "lightgreen"), alpha = 0.5) +
  ylab("Mean theta") +
  xlab("Date")+
  theme(legend.position="bottom") +
  scale_fill_discrete(name= "Topics", labels =c("schnee_straßen_geschwindigkeit_geschwindigkeitskontrollen_gut_kontrollieren_eis",
                                               "betrüger_tipps_falsche_betrug_geld_achtung_vorsicht",
                                               "versammlung_verstöße_einhaltung_kontrollen_maskenpflicht_corona_gilt"))
toptime

####Topic Probabilities/Frequencies####
meantm <- summarise_if(tmjoin, is.numeric, mean) %>%
  t() %>%
  as.data.frame() %>%
  rename(meantheta = ("V1")) %>%
  slice(7:26)

meantm <- arrange(meantm, meantheta)

####Extract tweets scoring high on specific topics####
htversammlung <- filter(tmjoin, versammlung > 0.97) %>% arrange(desc(versammlung))
write.csv(htversammlung, "VersammlungTopicScoringAbsteigend.csv")
