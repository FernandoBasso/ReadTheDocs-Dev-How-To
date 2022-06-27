export const NAME = "l42b Low Level Utilities";

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

type FetchDBKind =
    "orders"
  | "products"
  | "customers";

type FetchDBReturn<T> =
  T extends "orders" ? Order[] :
  T extends "products" ? Product[] :
  T extends "customers" ? Customer [] :
  never;

function isAvailable<Thing>(
  param: Thing,
): param is NonNullable<Thing> {
  return typeof param !== "undefined" && param !== null;
};

//
// A utility type that retrieves the resolved value type of a
// promise.
//
type Unpack<T> =
  T extends Promise<infer Res> ? Res : never;

declare function fetchFromDb<
  Kin extends FetchDBKind
>(
  kin: Kin,
): Promise<FetchDBReturn<Kin> | null>;

function process<T extends Promise<any>>(
  promise: T,
  cb: (res: Unpack<NonNullable<T>>) => void,
): void {
  promise.then(res => {
    if (isAvailable(res)) cb(res);
  });
}

declare function listOrders(orders: Order[]): void;

process(
  fetchFromDb("orders"),
  listOrders,
);
