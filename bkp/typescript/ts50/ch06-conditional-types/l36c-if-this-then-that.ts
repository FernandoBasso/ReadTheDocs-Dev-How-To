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

type FetchReturn<T> = T extends Customer ? Order[]
  : T extends Product ? Order[]
  : T extends number ? Order
  : never;

declare function fetchOrder<T extends number | Customer | Product>(
  params: T
): FetchReturn<T>;

declare function fetchP(customer: Customer): Order[];
declare function fetchP(product: Product): Order[];
declare function fetchP(orderId: number): Order;
declare function fetchP(param: any): Order | Order[];

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
