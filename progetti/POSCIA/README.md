#IMPLEMENTAZIONE DI UN ATTACCO BOOMERANG SU CIFRARIO SKINNY RIDOTTO
Il cifrario in questione viene implemenatato in SKINNY64 (la versione a 64 bit), gli esempi applicativi invece sono presenti in TESTVECTOR
L'attacco invece è su 6 round dello Skinny, 2 per l'upper part, 2 di middle part, 1 per la lower part e un round aggiuntivo per il key ricovery.
Per riuscire in questo scopo sono presenti nella directory 3 script:
1_FIND_DIFFERENTIAL_TRAIL: lo scopo è appunto quello di trovare i differenziali più vantaggiosi per l'attacco, questo programma è a suo volta composto da più parti, 
una prima parte in cui si risolve un problema di tipo MILP per trovare i differenziali troncati, la seconda e la terza parte invece vanno a determinare i differenziali
effettettivi costruendo dei grafi e poi, utilizzando un algoritmo per il cammino di costo minimo determina quelli che massimizzano le probabilità p e q (probabilità per la parte superiore
e inferiore del cifrario), infine viene determinata r (probabilità legata alla parte mediana) sperimentalmente, ottenendo in fine i 4 differenziali utili all'attacco vero e proprio, i risultati
sono stati salvati in ris_diff_trail;
2_Boomerang_Probability, qui viene calcolata la probabilità complessiva di chiusura del boomerang, salvando i quartetti validi in ris_B_pro(Attenzione:il tempo di esecuzione è di circa 30 minuti);
3_KEY_RICOVERY, viene fatto il key ricovery sfruttando i quartetti trovati in precedenza;
