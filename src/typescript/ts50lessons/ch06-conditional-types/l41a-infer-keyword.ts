export const NAME = "l41a The Infer Keyword";

//
// Roles:
//
// • admin: system god.
// • maint: allowed to modify products.
// • ship: allowed to read orders
//

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
