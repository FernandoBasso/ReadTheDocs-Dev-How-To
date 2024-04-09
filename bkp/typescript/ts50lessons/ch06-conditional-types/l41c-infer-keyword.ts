export const NAME = "l41c The Infer Keyword";

//
// Unsafe, but we are in development mode.
//
let userId = 0;

function createUser(
  name: string,
  roles: "admin" | "maint" | "ship",
  isActive: boolean,
) {
  return {
    userId: userId++,
    name,
    roles,
    isActive,
    createdAt: new Date(),
  };
}

//
// The updated version using the ‘infer’ keyword.
//
// We simply replaced
//
//   any ? any
//
// with
//
//   infer R ? R
//
type GetReturn<Fn> =
  Fn extends (...args: any[]) => infer R ? R : never;

type User = GetReturn<typeof createUser>;
//
// Only the return type of the function is returned.
//
//   type User = {
//     userId: number;
//     name: string;
//     roles: "admin" | "maint" | "ship";
//     isActive: boolean;
//     createdAt: Date;
//   }
//
