export const NAME = "l38a Distributive Conditionals";


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

type FetchReturn<Param> = Param extends Customer ? Order[]
  : [Param] extends [Product] ? Order[]
  : [Param] extends [number] ? Order
  : never;

//
// For single type bindings, the conditional type works
// as before.
//
type FetchByCustomer = FetchReturn<Customer>;
//   ^?

type FetchByCustomer_1 =
    //
    // This condition is still true.
    //
    [Customer] extends [Customer] ? Order[]
  : [Customer] extends [Product] ? Order[]
  : Order;

var r1: FetchByCustomer_1;
//  ^?


//
// But when we instantiate ‘Param’ with a union type, and this
// doesn't get distributed...
//
type FetchByCustomerOrId = FetchReturn<Customer | number>;
//   ^?

type FetchByCustomerOrId_1 =
  //
  // This is false.
  //
  [Customer | number] extends [Customer] ? Order[]

  //
  // This is also false.
  //
  : [Product | number] extends [Product] ? Order[]

  //
  // So we must resolve to this...
  //
  : Order;

var r2: FetchByCustomerOrId_1;
//  ^?
//
// Shouldn't be ‘Order‘. Gasp.
//

//
// But we can make it safer and more correct by checking
// for the subtype of ‘number’.
//
type FetchReturnCorrect<Param extends FetchParams> =
    [Param] extends [Customer] ? Order[]
  : [Param] extends [Product] ? Order[]
  : [Param] extends [number] ? Order
  : never;

type c1 = FetchReturnCorrect<Customer>;

type c2 = FetchReturnCorrect<Product>;

type c3 = FetchReturnCorrect<number>;

type c4 = FetchReturnCorrect<Order | number>;

