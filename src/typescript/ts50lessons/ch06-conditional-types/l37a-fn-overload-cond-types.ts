export const NAME = "l37a Function Overloads and Conditional Types";

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

type Callback<T> = (result: T) => void;

function fetAs<T extends FetchParams>(inp: T): Promise<FetchReturn<T>>;
function fetAs<T extends FetchParams>(
  inp: T,
  fun: Callback<FetchReturn<T>>
): void;

function fetAs<T extends FetchParams>(
  inp: T,
  fun?: Callback<FetchReturn<T>>
): Promise<FetchReturn<T>> | void {
  const res = fetch("url").then((res) => res.json());
  if (fun) {
    res.then((result) => {
      fun(result);
    });
  } else {
    return res;
  }
}
