#include <stdio.h>

/**
 * Accepts a comma-separated list of values and produces
 * the json output.
 *
 * Example usage:
 *
 *   $ ./0dev < ./gpsdata.csv > output.json
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

      if (-90 > lat || lat > 90) {
        fprintf (stderr, "Invalid latitude: %f\n", lat);
        return 2;
      }
      if (-180 > lat || lon > 180) {
        fprintf (stderr, "Invalid longitude: %f\n", lon);
        return 2;
      }
    }

    printf ("\t{latitude: %f, longitude: %f, info: '%s'}",
            lat, lon, info);

  }

  puts ("\n]");

  return 0;
}

/* vim: set syn=off ft=text ai: */

