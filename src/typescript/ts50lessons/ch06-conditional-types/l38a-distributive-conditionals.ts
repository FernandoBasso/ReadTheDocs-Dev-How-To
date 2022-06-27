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

type FetchReturn<T> = T extends Customer ? Order[]
  : T extends Product ? Order[]
  : T extends number ? Order
  : never;


type FetchByCustomer = FetchReturn<Customer>;

//
// Expanding the above:
//
type FetchByCustomer_1 =
    Customer extends Customer ? Order[]
  : Customer extends Product ? Order[]
  : Order;


//
// Distribution over Unions
// ========================
//

type FetchByProductOrId = FetchReturn<Product | number>;

type FetchByProductOrId_1 = (
      Product extends Customer ? Order[]
    : Product extends Product ? Order[]
    : Order
  ) | (
      number extends Customer ? Order[]
    : number extends Product ? Order[]
    : Order
  );


type FetchByProductOrCustomerOrId = FetchReturn<Product | Product | number>;

//
// The above is the same as this:
//
type FetchByProductOrCustomerOrId_1 = (
      Product extends Customer ? Order[]
    : Product extends Product ? Order[]
    : Order
  ) | (
      Customer extends Customer ? Order[]
    : Customer extends Product ? Order[]
    : Order
  ) | (
      number extends Customer ? Order[]
    : number extends Product ? Order[]
    : Order
  );

//
// Is equal to:
//
type FetchByProductOrCustomerOrId_2 = Order[] | Order[] | Order;

//
// Remove redundancies:
//
type FetchByProductOrCustomerOrId_3 = Order[] | Order;
