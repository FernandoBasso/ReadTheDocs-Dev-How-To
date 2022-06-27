export const NAME = "l36a If This, Then That";

const log: Console["log"] = console.log.bind(console);

type Customer = {
  customerId: number;
  firstName: string;
  lastName: string;
};

type Product = {
  productId: number;
  title: string;
  price: number;
};

type Order = {
  orderId: number;
  customer: Customer;
  products: Product[];
  date: Date;
}

//
// Structurally matches the type ‘Customer’.
//
const customer = {
  customerId: 1,
  firstName: "Aayla",
  lastName: "Secura",
};

//
// Structurally matches the type ‘Product’.
//
const product = {
  productId: 1,
  title: "Lightsaber",
  price: 29,
};

//
// A ‘fetchOrder()’ function that:
//
// • given a customer, fetch orders from that customer;
// • given a product, fetch orders that include that product;
// • given an order id, fetch the order itself.
//
// We could use overloads...
//
function fetchOrder(customer: Customer): Order[];
function fetchOrder(product: Product): Order[];
function fetchOrder(orderId: number): Order;
function fetchOrder(param: any): any {
  // Logic to implement overload...
}

//
// Works well for simple cases where we absolutely know which
// params to expect.
//
const a = fetchOrder(customer);
const b = fetchOrder(product);
const c = fetchOrder(1);

declare const hmm: Customer | number;
const result = fetchOrder(hmm);
