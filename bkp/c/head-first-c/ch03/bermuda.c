#include <stdio.h>

int main ()
{
  float lat, lon;
  char info[80];

  while (scanf ("%f,%f,%79[^\n]", &lat, &lon, info) == 3) {
    /* Read “if lat is above 26 and below 34.” */
    if (26 < lat && lat < 34) {
      /* Read “if lon is above -76 and below -64.” */
      if (-76 < lon && lon < -64) {
        printf ("%f,%f,%s", lat, lon, info);
      }
    }
  }

  return 0;
}

/* vim: set syn=off ft=text ai: */
