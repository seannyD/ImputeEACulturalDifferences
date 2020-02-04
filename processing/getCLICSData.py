import sqlite3,itertools

# Connect to clics
connection = sqlite3.connect("clics.sqlite")
cursor = connection.cursor()

#cursor.execute("SELECT name FROM sqlite_master WHERE type='table';")
#print(cursor.fetchall())
#[('dataset',), ('datasetmeta',), ('SourceTable',), ('LanguageTable',), ('ParameterTable',), ('FormTable',), ('CognateTable',), ('CognateSource',), ('FormSource',), ('BorrowingTable',), ('ValueTable',), ('ValueSource',), ('BorrowingSource',)]



# Load word2vec data

w2vLangs = ["nort2697","bela1254","bulg1262","stan1318","stan1289","czec1258","dani1285","mode1248","stan1288","stan1295","esto1258","stan1293","basq1248","west2369","finn1318","stan1290","hind1269","hebr1245","croa1245","hung1274","nucl1235","nucl1643","nucl1302","kaza1248","ital1282","kore1280","lith1251","dutc1256","norw1258","poli1260","port1283","roma1327","slov1269","russ1263","tami1289","nucl1301","swed1254","nort2690","ukra1253","mand1415","slov1268"]

# Get list of languages in data

NELConcepts = ["Hose::N","Zeichen::N","Lärm::N","Ruf::N","ehemalig::A","sanft::A","Boden::N","Gesicht::N","Rand::N","heiß::A","Art::N","Korn::N","Muster::N","Kragen::N","klug::A","Herz::N","Ort::N","Figur::N","Falle::N","Geruch::N","kaum::ADV","dicht::A","Griff::N","Wurzel::N","Seite::N","Nebel::N","Morgen::N","hungrig::A","Ufer::N","Angelegenheit::N","richtig::A","Heim::N","wahr::A","Bogen[Waffe]::N","Platz::N","Rauch::N","Reihe::N","Geschmack::N","Zaun::N","Raureif::N","dann::ADV","Tante::N","Geschenk::N","Regal::N","Band::N","Schlitten::N","sauber::A","vorher::ADV","bald::ADV","Hälfte::N","gut::A","das::PRN","Name::N","sofort::ADV","Rätsel::N","Gelächter::N","Stück::N","Alter::N","Platte::N","Rohr::N","Leute::N","nass::A","Bart::N","wegen::PRP","tausend::NUM","Gang::N","Sache::N","fremd::A","Atem::N","hierhin::ADV","verschieden::A","Essen::N","oder::CNJ","bunt::A","zusammen::ADV","Haus::N","Gast::N","Kampf::N","Feder::N","Weide[Baum]::N","Grund::N","Neuigkeit::N","Gespräch::N","Asche::N","Feind::N","Gold::N","Gürtel::N","Fußboden::N","Faden::N","Welt::N","Gegenstand::N","Haufen::N","Wange::N","Schuh::N","heute::ADV","leer::A","Menge::N","Wunde::N","Strich::N","frisch::A","schlecht::A","Ast::N","Schwanz::N","flach::A","Beere::N","Stoff::N","jung::A","nah::A","lebendig::A","Linie::N","Nachricht::N","Haken::N","dünn::A","Bett::N","Stütze::N","unter::PRP","schwer::A","Hitze::N","Schritt::N","Schatten::N","böse::A","schlimm::A","er::PRN","Ofen::N","Schaufel::N","Handtuch::N","Ende::N","nackt::A","neben::PRP","Erzählung::N","Bein::N","Grenze::N","Oberschenkel::N","zwischen::PRP","niemals::ADV","Kind::N","Kopf::N","Gras::N","Besen::N","lange::ADV","Herbst::N","Anzahl::N","roh::A","lieb::A","vor::PRP","gestern::ADV","Fluss::N","Bild::N","Sack::N","Silber::N","fett::A","Schluss::N","zuerst::ADV","Pferd::N","fleißig::A","Traum::N","Leder::N","breit::A","Anhöhe::N","schlank::A","Kreis::N","Lehrer::N","Ruhe::N","Handfläche::N","Scheibe::N","Strömung::N","Laut::N","Unglück::N","Wind::N","offen::A","lang::A","Ware::N","Lied::N","reif::A","schon::ADV","Klaue::N","Wurm::N","groß::A","rund::A","Stock::N","Nagel::N","blau::A","Gewicht::N","Hund::N","Tee::N","fern::A","klein::A","zurück::ADV","Herde::N","Sehne::N","Kissen::N","vorwärts::ADV","Nutzen::N","Knopf::N","berühmt::A","Deckel::N","Leiter::N","Tanne::N","Schaden::N","Ferse::N","Boot::N","geschlossen::A","Meister::N","Leben::N","Knie::N","Volk::N","dick[Gegenstand]::A","Kamm::N","Fehler::N","Spur::N","Fisch::N","König::N","glatt::A","Laus::N","Nadel::N","Haar::N","Tropfen::N","Kessel::N","hart::A","Birke::N","Gott::N","Arbeit::N","Verstand::N","Krieg::N","Tuch::N","Topf::N","Arm::N","fest::A","Fett::N","Öl::N","Gegend::N","Chef::N","Arznei::N","schmutzig::A","Brett::N","weich::A","Geist::N","Hügel::N","Spitze::N","Preis::N","fein::A","Kirche::N","Lehm::N","Wahrheit::N","Blume::N","wieder::ADV","Tasse::N","Schlaf::N","Nabel::N","noch::ADV","Dorf::N","so::ADV","Sumpf::N","Kreuz::N","billig::A","Zunge::N","Sprache::N","Zahn::N","alt[Lebewesen]::A","Messer::N","Huhn::N","Ärmel::N","Hunger::N","Himmel::N","sieben::NUM","Staub::N","oben::ADV","sauer::A","Arzt::N","Osten::N","Flügel::N","Spaten::N","Nacht::N","Stimme::N","Ton::N","Sand::N","Weide::N","Kleidung::N","unten::ADV","Elch::N","schütten::V","Fliege::N","warm::A","Rinde::N","sehr::ADV","Größe::N","Abend::N","Magen::N","Familie::N","Kiefer[Anatomie]::N","Meer::N","Kralle::N","Kiste::N","Schloss::N","trocken::A","Großvater::N","Brief::N","einst::ADV","Schmetterling::N","Eisen::N","Wolke::N","Wunsch::N","Dach::N","Fenster::N","Schuld::N","Festland::N","Speise::N","teuer::A","Knoten::N","rückwärts::ADV","Last::N","Geschäft::N","Mädchen::N","Tochter::N","Zeitung::N","Reichtum::N","Stadt::N","Ei::N","sich drehen::V","hinauf::ADV","Salz::N","Ohr::N","Milch::N","Nase::N","Blatt::N","anderer::A","Tor::N","schwach::A","Adler::N","Bauch::N","Baum::N","Frühling::N","Arbeiter::N","Frau::N","Brunnen::N","spitz::A","Eimer::N","Löffel::N","Zwiebel::N","Kuckuck::N","Fieber::N","Luft::N","Wetter::N","Dreck::N","und::CNJ","bitter::A","Wasser::N","Quelle::N","Glas::N","Geld::N","Honig::N","traurig::A","Glück::N","Haut::N","Apfel::N","Blut::N","Junge::N","Mond::N","Monat::N","Buchstabe::N","Knochen::N","Mücke::N","lecker::A","sie::PRN","Wolle::N","Ameise::N","neu::A","Schwester::N","lieben::V","Rücken::N","Insel::N","Rede::N","Bruder::N","Mensch::N","was::FPRN","Tier::N","Kranich::N","Gehirn::N","Brot::N","Gedanke::N","Finger::N","Großmutter::N","Loch::N","Buch::N","Ellenbogen::N","bauen::V","Jahr::N","kühl::A","hundert::NUM","Fuß::N","wo::FADV","wertvoll::A","Stein::N","Sünde::N","Eichhörnchen::N","Abstand::N","dort::ADV","Wiege::N","Woche::N","Ecke::N","Sonne::N","süß::A","wir::PRN","ich::PRN","vierzig::NUM","kalt::A","Stern::N","eins::NUM","Schnee::N","wohin::FADV","Onkel::N","Zeit::N","Katze::N","gelb::A","Bär::N","kräftig::A","schwarz::A","Eis::N","Mutter::N","weiß::A","warum::FADV","kommen::V","ihr::PRN","Tag::N","Entfernung::N","Schlange[Tier]::N","machen::V","kurz::A","Kraft::N","Norden::N","krank::A","springen::V","drei::NUM","Hase::N","du::PRN","wer::FPRN","Sommer::N","schreiben::V","Süden::N","stark::A","schlagen::V","wieviel::FNUM","wie::FADV","Staat::N","grün::A","Maus::N","Vogel::N","Länge::N","Winter::N","hauen::V","bekommen::V","erhalten::V","nehmen::V","reichen::V","Vater::N","Sonntag::N","geben::V","Krankheit::N","reich::A","achtzig::NUM","Hilfe::N","letzter::A","Stärke::N","rot::A","Fuchs::N","sechzig::NUM","Mittwoch::N","zwanzig::NUM","Samstag::N","fünf::NUM","zwei::NUM","erster::A","sechs::NUM","acht::NUM","zweiter::A","dritter::A","blind::A","danach::ADV","Ruder::N","Schwarm::N","geradeaus::ADV","dumm::A","Kamerad::N","loben::V","ziehen::V","brennen::V","Ring::N","fließen::V","Barsch::N","geizig::A","reifen::V","Nahrung::N","hübsch::A","genesen::V","schließen::V","anhalten::V","Winkel::N","plötzlich::ADV","Gedächtnis::N","öffnen::V","Tasche::N","hell::A","manchmal::ADV","hinter::PRP","Teil::N","taub::A","faul::A","Schnurrbart::N","stehen::V","anschauen::V","Puppe::N","bleiben::V","Mitte::N","Tisch::N","sich setzen::V","halten::V","verlieren::V","Hand::N","allein::ADV","messen::V","hineingehen::V","eintreten::V","Schnur::N","scharf::A","Schulter::N","geschickt::A","Gabel::N","Holz::N","wissen::V","durch::PRP","Freude::N","abschneiden::V","Decke::N","aufhören::V","bringen::V","wollen::V","Schmerz::N","untergehen[Sonne]::V","strömen::V","Hemd::N","streichen::V","Zweig::N","Stamm::N","laufen::V","zurücklassen::V","versuchen::V","Stiefel::N","schneiden::V","schmal::A","aufheben::V","zurückkommen::V","versinken::V","Gans::N","Riemen::N","geraten::V","abreißen::V","Netz::N","Laden::N","Frost::N","Kehle::N","zu sehen sein::V","können::V","legen::V","lachen::V","bekannt::A","Horn::N","hinab::ADV","besitzen::V","Bündel::N","sich erheben::V","küssen::V","beugen::V","fahren::V","Land::N","liegen::V","Spiel::N","scheinen::V","leben::V","spielen::V","Brei::N","Lust::N","zählen::V","stumpf::A","stehlen::V","ansehen::V","Gewehr::N","Beutel::N","heben::V","vergehen::V","gießen::V","Schaum::N","Gewalt::N","dunkel::A","Sinn::N","jagen::V","verteidigen::V","Wiese::N","Hals::N","anzünden::V","schlucken::V","auf einmal::ADV","Auge::N","erscheinen::V","hören::V","Pilz::N","Suppe::N","beenden::V","hochheben::V","treffen::V","Mütze::N","sagen::V","Lippe::N","Grab::N","beschädigen::V","bezahlen::V","zahlen::V","verbessern::V","immer::ADV","sich fürchten::V","gehen::V","bewahren::V","abwischen::V","Farbe::N","wiegen::V","arbeiten::V","anfangen::V","wohnen::V","schenken::V","Grube::N","Nest::N","weinen::V","Spinne::N","sterben::V","bitten::V","aufbewahren::V","tauchen::V","Saat::N","Wort::N","fliehen::V","flüchten::V","alt::A","Kälte::N","Märchen::N","Mund::N","wachsen::V","hüten::V","Ader::N","Stirn::N","schicken::V","wechseln::V","Stuhl::N","Busen::N","suchen::V","begreifen::V","steigen::V","vergessen::V","lesen::V","stoßen::V","Pfeil::N","Spiegel::N","Kinn::N","Hahn::N","füttern::V","warten::V","hinfallen::V","Krähe::N","morgen::ADV","essen::V","Kohle::N","beginnen::V","fühlen::V","sehen::V","mitteilen::V","vernehmen::V","schreien::V","Weg::N","Höhle::N","drehen::V","Kuh::N","verstecken::V","nennen::V","fliegen::V","fragen::V","tanzen::V","Brücke::N","Freund::N","Fell::N","betrügen::V","Feuer::N","singen::V","Macht::N","werfen::V","schießen::V","finden::V","eilen::V","Wolf::N","feucht::A","fangen::V","jetzt::ADV","Küste::N","rennen::V","Lüge::N","abnehmen::V","sitzen::V","schön::A","töten::V","passieren::V","wünschen::V","Mann::N","wecken::V","denken::V","sammeln::V","tun::V","Genick::N","Pfad::N","Tür::N","dies::PRN","ändern::V","von hier::ADV","sich erinnern an::V","werden::V","Hecht::N","Bulle::N","Tod::N","sich anziehen::V","Abhang::N","lustig::A","Höhe::N","Welle::N","lernen::V","Brust::N","anziehen::V","später::ADV","glauben::V","voll::A","zuhören::V","lehren::V","unterrichten::V","Gesundheit::N","nur::ADV","See::N","sich versammeln::V","Moor::N","Bucht::N","Ente::N","Eule::N","Schaf::N","grau::A","Fleisch::N","sprechen::V","dorthin::ADV","Eltern::N","Getreide::N","Ehemann::N","Licht::N","Kiefer[Baum]::N","reden::V","Schwein::N","Ehefrau::N","Berg::N","Gipfel::N","Regen::N","Kummer::N","linker::A","Sohn::N","Mittag::N","Wald::N","hier::ADV","rechter::A","arm::A","siebzig::NUM","fünfzig::NUM","dreißig::NUM","zehn::NUM","neun::NUM","Westen::N","vier::NUM","schieben::V","schützen::V","sich schlagen::V","müde werden::V","verkaufen::V","sinken::V","stecken::V","spazierengehen::V","stellen::V","wählen::V","binden::V","Bau::N","sich beeilen::V","ertragen::V","nähen::V","teilen::V","drücken::V","umdrehen::V","beißen::V","verderben::V","biegen::V","verbrennen::V","einwickeln::V","vereinigen::V","pflücken::V","berühren::V","hacken::V","kochen::V","aufhängen::V","schmücken::V","braten::V","erkennen::V","stechen::V","Stab::N","stricken::V","fassen::V","Funke::N","Kante::N","setzen::V","vorbereiten::V","bedecken::V","zudecken::V","brechen::V","rufen::V","sich erholen::V","verstehen::V","baden::V","zeigen::V","rechnen::V","klopfen::V","kämmen::V","waschen::V","zerbrechen::V","füllen::V","Straße::N","Ski::N","backen::V","anbinden::V","weggehen::V","sich waschen::V","zerstören::V","reinigen::V","färben::V","Schale::N","neunzig::NUM","packen::V","umrühren::V","wenden::V","tragen::V","verbinden::V","ergreifen::V","sich trennen::V","läuten::V","täuschen::V","stören::V","zubereiten::V","enden::V","trocknen::V","verbergen::V","Schmutz::N","ein wenig::ADV","Gesellschaft::N","Erde::N","Regenbogen::N","Lagerfeuer::N","Heu::N","vergebens::ADV","herstellen::V","Mai::N","kaufen::V","Freitag::N","August::N","Donnerstag::N","Dienstag::N","März::N","Montag::N","prahlen::V","fürchten::V","klingen::V","probieren::V","ausziehen::V","aufgehen[Sonne]::V","führen::V","Oktober::N","hinzufügen::V","kehren::V","alles::PRN","fallen::V","aufstehen::V","Bach::N","riechen::V","erwarten::V","siegen::V","schmerzen::V","schleppen::V","einladen::V","mögen::V","übersetzen::V","oft::ADV","hängen::V","sich unterhalten::V","leiten::V","aufziehen::V","geboren werden::V","trinken::V","umkommen::V","beherrschen::V","aufwachen::V","Leber::N","Zeh::N","schmelzen::V","bewachen::V","schwimmen::V","graben::V","treiben::V","verlassen::V","retten::V","schlafen::V","zunehmen::V","sich bewegen::V","auswählen::V","ankommen::V","bemerken::V","Körper::N","Butter::N","treten::V","erzählen::V","krank sein::V","nicht::ADV","Schwan::N","krank werden::V","Dezember::N","Januar::N","Februar::N","September::N","Juni::N","Juli::N","November::N","April::N","malen::V","behindern::V","überall::ADV","Donner::N","atmen::V","versprechen::V","scheinen[Sonne]::V","verschwinden::V","damals::ADV","wehen::V","zwölf::NUM","elf::NUM","schreiten::V","klingeln::V","klettern::V","reparieren::V","zittern::V","besuchen::V","einschlafen::V","lassen::V","Leine::N","Träne::N","Geschirr::N","Fingernagel::N","Nagel[Anatomie]::N","brausen::V","einmal::ADV","gelingen::V","gefrieren::V","bellen::V","rühren::V","fallen lassen::V","reiben::V","heulen::V","Stange::N","kleben::V","schimpfen::V","melken::V","sausen::V","zeichnen::V","pfeifen::V","schleifen::V","schwenken::V","sich ärgern::V","rütteln::V","sieden::V","schwingen::V","schütteln::V","reißen::V","fegen::V","kriechen::V","blasen::V","schaukeln::V","faulen::V","lärmen::V","sich rühren::V","zerreißen::V","glänzen::V","stöhnen::V","antreiben::V","donnern::V","verfaulen::V","funkeln::V","belästigen::V","rauchen::V","spülen::V","fischen::V","schwanken::V","husten::V","lecken::V","kratzen::V","abfahren::V","regnen::V","tauen::V","Schlinge::N","rauschen::V","rudern::V","ärgern::V","klirren::V","frieren::V"]


# Get list of concepts in data
selCommand = """
SELECT DISTINCT Concepticon_Gloss
  FROM ParameterTable
  WHERE NorthEuralex_Gloss in (CONLIST)
  """

conlist = "'" + "','".join(NELConcepts) + "'"
selCommand = selCommand.replace("CONLIST",conlist)

#print(selCommand)

cursor.execute(selCommand)
result = cursor.fetchall() 

concepts = [x[0] for x in result]
conlist = "'" + "','".join(concepts) + "'"

langlist = "'" + "','".join(w2vLangs) + "'"

#print(conlist)
	

selCommand = """
SELECT l.Glottocode, p.Concepticon_Gloss, f.Parameter_ID, f.Form 
  FROM FormTable f 
  LEFT JOIN LanguageTable l ON f.Language_ID = l.ID 
  LEFT JOIN ParameterTable p ON f.Parameter_ID = p.ID 
  WHERE l.Glottocode in (LANGLIST) AND p.Concepticon_Gloss in (CONLIST)
  """

selCommand = selCommand.replace("LANGLIST",langlist)
selCommand = selCommand.replace("CONLIST",conlist)

#print(selCommand)

cursor.execute(selCommand) 
result = cursor.fetchall() 

# Read into dictionary
#{langA: conceptA: ['one','two']}
# while making list of concepts

d = {}
concepts = []
for r in result:
	lang = r[0]
	con = r[1]
	conid = r[2]
	form = r[3]
	if not lang in d.keys():
		d[lang] = {}
	if not con in d[lang].keys():
		d[lang][con] = []
	d[lang][con].append(form)
	
	if not con in concepts:
		concepts.append(con)
		
langs = d.keys()

#  account for multiple words per concept
#  by making sets and checking for intersection
for l in d.keys():
	for c in d[l].keys():
		d[l][c] = set(d[l][c])


print("l1,l2,numBothColexified,numMatchingColexifications,numOverlappingConcepts,numConceptPairs")
langPairs = {}
for l1,l2 in list(itertools.combinations(langs, r=2)):
	l1Con = set(d[l1].keys())
	l2Con = set(d[l2].keys())
	overlapConcepts = l1Con & l2Con
	overlapConceptPairs = list(itertools.combinations(overlapConcepts, r=2))
	numColexified = 0
	numMatchingColexified = 0
	for c1,c2 in overlapConceptPairs:
		l1Colexified = bool(d[l1][c1] & d[l1][c2])
		l2Colexified = bool(d[l2][c1] & d[l2][c2])
		# 1 if both are colexified
		# la  |  lb  |  result 
		#  T  |  T   |    1
		#  F  |  F   |    0
		#  T  |  F   |    0
		#  F  |  T   |    0
		numColexified += l1Colexified & l2Colexified
		numMatchingColexified += l1Colexified == l2Colexified
			
	print(",".join([l1,l2,str(numColexified),str(numMatchingColexified),str(len(overlapConcepts)),str(len(overlapConceptPairs))]))


