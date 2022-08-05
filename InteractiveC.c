#include <stdio.h>
#include <string.h>

int Berechnungen();
void printArray();
int g = 6;

struct Videos {
  int laenge;
  char autor[50];
} video;

void printVideo( struct Videos *vid );

/* Here you can see how to define a constant thats read-only */
#define PI 3.14

int main() {


  struct Videos Video1;
  Video1.laenge = 10;
  strcpy(Video1.autor, "Pointer");

  /* Pointer auf structures setzen aus Effizienzgründen */
  printVideo( &Video1 );

  /* Variable als char */
  char x = 'c';
  /* Variable als int */
  int y;
  y = 9;
  /* Variable als float hier double für doppelte Genauigkeit*/
  double z = y + x;
  /* printf zum ausgeben and die Konsole */
  printf("Hello World! %f %f\n", PI, z);

  int a = 3;
  int b = 3;

  /* modulo Operator */
  printf("a ist %d\n", a%b);

  /* a wird um 1 erhöht */
  a++;

  /* relationale Operatoren */
  if( b >= 0) {
    printf("b ist 0 oder größer 0\n");

  }
  else if( b <= 0 ) {
    printf("b ist 0 oder kleiner 0\n");
  }

  if( b < 0 || a == 4 ) {
    printf("a ist gleich 4 oder b ist kleiner als 0\n");
  }


  if ((b >= 0 || a ==4) && (b < 70 || !(a < -120))) {
    printf("lol\n");
  }

  /* If-Verzweigung in C */
  if (b == 0) {
    printf("b ist 0\n");
  }
  /* else if mit weiterer Bedingung */
  else if(b == 1) {
    printf("b ist 1\n");
  }
  /* else wird ausgeführt wenn keiner der voherigen
    Bedingungen zutrifft */
  else {
    printf("b ist nicht 0\n");
  }

  /* switch case Verrzweigung für Schulnoten
     man beachte das nach der richten Switch
     Case Verzweigung alle nachfolgenden
     Verzweigungen ausgeführt werden
     das kann zu Problemen aber auch zu Vorteilen führen */
  char note = 'B';
  switch(note) {
    case 'A':
      printf("Super!\n");
    case 'B':
      printf("Gut!\n");
    case 'C':
      printf("Befriedigend!\n");
      break;
    case 'D':
      printf("Ausreichend!\n");
      break;
    case 'F':
      printf("Ungenügend!\n");
      break;
    default:
      printf("das ist keine hier akzeptierte Note\n");
      break;
  }


  /* Pointer Basics */
  /* Dekalration eines Pointers für einen Integer*/
  int *p;
  /* Dem Pointer wird die Speicheradresse von Variable A zugewiesen*/
  p = &a;
  printf("Memory Adress of a is %p\n",p);
  /* Änderung des Inhalts für a, Zugriff auf Speicheradresse
     auf die gepointed wird und Änderung ihres Inhaltes */
  *p = 10;
  printf("sizeof integer is %d\n", sizeof(int));
  printf("a currently is %d\n", a);

  /* while Schleife in C */
  int s = 8;
  while(s > 0) {
    printf("s ist %d\n", s);
    s--;
  }

  int l = 0;
  while(s < 8) {
    while(l < 8) {
      l++;
      printf("l ist %d\n", l);
    }
    printf("s ist %d\n", s);
    s++;
  }

  /* for Schleife in C */
  int m;
  for(m = 0; m < 10; m++) {
    printf("m ist %d\n", m);
  }

  /* do-while schleife in C
     die Berechnung hinter do wird hier immer ausgeführt
     auch wenn die while Bedingung nie eintreffen wird */
  int k = 0;
  do {
    printf("k ist %d\n", k);
  } while (k > 0);

  /* Bei continue wird der Rest der Schleife ignoriert
     und direkt wieder zum Anfang gesprungen
     bei break wird gelich die ganze Schleife beendet und
     nicht wieder zum Anfang begeben */
  for (a = 0; a < 10; a++) {
    if (a == 5) {
      printf("panic!\n");
      continue;
    }
    printf("phew! Peng!\n");
  }

  int i = 51;
  if (50 < i && i < 80) {
    printf("ist zwischen 50 und 80\n");
  }

  int g = 8;
  printf("%d\n", g);

  printf("main\n");

  int d = Berechnungen(42, 1337);
  printf("main %d\n", d);

  int e[10];

  for (int i = 0; i < 10; i++) {
    e[i] = 24;
  }

  printArray(e, 10);

  int o[3] = {44, 34, 66};
  int u;
  int *ptr;

  ptr = o;
  printf("%d \n", *(ptr + 1));

  char String1[] = "Hi \0";
  char String2[] = "du";
  char String3[] = "Na?";

  strcpy(String3, String1);

  strcat(String3, String2);

  printf("%s\n", String3);
  printf("%ld\n", strlen(String2));

  int R2Array[3] [3] = {
    {5, 6, 7},
    {6, 6, 6},
    {7 ,7, 7}
  };

  return 0;
}

int Berechnungen(int v, int c) {
  printf("%d\n", g);
  int a = v * c;
  return a;
}

void printArray(int a[], int size) {

  for (int i = 0; i < size; i++) {

    printf("%d ", a[i]);
  }
  printf("\n");
}

void printVideo(struct Videos *vid){
  printf( "autor: %s\n", vid->autor );
  printf( "länge: %d\n", vid->laenge );
}
