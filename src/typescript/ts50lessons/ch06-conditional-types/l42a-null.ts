export const NAME = "l42a Working with null";

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
// Do we really always, definitely return a promise of
// ‘Order[]’‽ What if we get back some nullish value? Or
// a request error (‘never’)‽
//
declare function fetchOrderList_v1(
  input: Customer | Product,
): Promise<Order[]>;

//
// This is much closer to the truth.
//
declare function fetchOrderList_v2(
  input: Customer | Product,
): Promise<Order[] | null>;

//
// We check for nullish orders inside the function.
//
declare function listOrders_v1(
  orders: Order[] | null,
): void;

//
// We have to check for nullish orders before attempting to
// pass them to the function.
//
declare function listOrders_v2(
  orders: Order[],
): void;

function isAvailable<Thing>(
  param: Thing,
): param is NonNullable<Thing> {
  return typeof param !== "undefined" && param !== null;
};

declare const customer: Customer;

//
// @TODO: Type is always ‘Order[]’, even though the function
// says it could return ‘null’. I guess it has something to do
// with using ‘declare’ in this case...
//
const orders = await fetchOrderList_v2(customer);

if (isAvailable(orders)) {
  listOrders_v2(orders);
}
