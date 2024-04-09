export const NAME = "l36b If This, Then That";

//
// This is the code from the TS playground. I could not yet
// create an implementation that works and the book doesn't
// seem to provide one.
//
// I have been given this link by mkantor at Discord TS server:
//
// • https://github.com/microsoft/TypeScript/issues/33014
//

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
};

type FetchParams = number | Customer | Product;

type FetchReturn<T> = T extends Customer
  ? Order[]
  : T extends Product
  ? Order[]
  : T extends number
  ? Order
  : never;

//
// Being explicit gets verbose very quickly. Seven overloads
// for for three possible input types and two possible output
// types.
//
function fetchOrder(customer: Customer): Order[];
function fetchOrder(product: Product): Order[];
function fetchOrder(orderId: number): Order;
function fetchOrder(param: Customer | Product): Order[];
function fetchOrder(param: Customer | number): Order[] | Order;
function fetchOrder(param: Product | number): Order[] | Order;
function fetchOrder(param: Customer | Product | number): Order[] | Order {
  // I hope I didn’t forget anything
}

const customer = {
  customerId: 1,
  firstName: "Stefan",
  lastName: "Baumgartner",
};

const product = {
  productId: 2,
  title: "Form Design Patterns",
  price: 29,
};

fetchOrder(customer);
fetchOrder(product);
fetchOrder(2);

declare const x: Customer | number;

fetchP(x);
fetchOrder(x);
