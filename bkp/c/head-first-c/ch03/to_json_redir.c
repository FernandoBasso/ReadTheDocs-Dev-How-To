#include <stdio.h>

/**
 * Accepts a comma-separated list of values and produces
 * the json output.
 *
 * Example usage:
 *
 *   $ ./0dev < ./gpsdata.csv > output.json
 *
 * Or even using ‘tee’, so you can see the data and still
 * also send it to ‘output.json’:
 *
 *  $ ./0dev < ./gpsdata.csv | tee ./output.json
 */


int main ()
{
  float lat,
        lon;

  char info[80];

  short started = 0;

  puts ("data=[");

  while (scanf ("%f,%f,%79[^\n]", &lat, &lon, info) == 3) {
    if (started) {
      printf (",\n");
    }
    else {
      started = 1;
    }

    printf ("\t{latitude: %f, longitude: %f, info: '%s'}",
            lat, lon, info);

  }

  puts ("\n]");

  return 0;
}

/* vim: set syn=off ft=text ai: */

