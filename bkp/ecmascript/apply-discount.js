const log = console.log.bind(console);

const products = [
  { title: 'Lightsaber', price: 1000, discountPercentage: 10 },
  { title: 'Darth Vader Helmet', price: 400, discountPercentage: 7 },
  { title: 'Millennium Falcon', price: 750000, discountPercentage: 12 },
];

/**
 * Creates a new `discount` and `totalToPay` properties on each product.
 *
 * @param {object} product
 * @return {object}
 */
const calculateDiscount = product => {
  const { price, discountPercentage } = product;
  const discount = price * discountPercentage / 100;
  const totalToPay = price - discount;
  return { ...product, discount, totalToPay };
};

// Useless use of an anonymous function award!
const updatedProducts = products.map(product => calculateDiscount(product));
log(updatedProducts);

const res = products.map(calculateDiscount);
console.info(res);


