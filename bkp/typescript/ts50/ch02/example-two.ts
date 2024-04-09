export const NAME = "example two";

const log: Console["log"] = console.log.bind(console);

/**
 * Adds value added tax (VAT) to a price.
 *
 * @param price
 * @param vat The tax as a decimal. E.g.: 7% is 0.07.
 * @returns
 */
function addVAT(price: number, vat = 0.2): number {
  return price * (1 + vat);
}

const vatPriceWith7Percent = addVAT(30);

// They occur'


if (window.isDev)
  log("isDev", window.isDev);
else
  log("no dev", window.isDev);


