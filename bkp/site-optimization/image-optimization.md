# Image Optimization

## Optimize PNG

```shell
$ optipng -o7 file.png
```

One real example:

```
proghowto [devel % u+1]
$ optipng -o7 haskell/imgs/2021-08-06-08-52.png 
** Processing: haskell/imgs/2021-08-06-08-52.png
612x499 pixels, 4x8 bits/pixel, RGB+alpha
Reducing image to 3x8 bits/pixel, RGB
Input IDAT size = 81784 bytes
Input file size = 82921 bytes

Trying:
  zc = 9  zm = 9  zs = 0  f = 0        IDAT size = 54609

Selecting parameters:
  zc = 9  zm = 9  zs = 0  f = 0        IDAT size = 54609

Output IDAT size = 54609 bytes (27175 bytes decrease)
Output file size = 55698 bytes (27223 bytes = 32.83% decrease)
```
