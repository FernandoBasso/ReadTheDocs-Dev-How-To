export const NAME = "l41b The Infer Keyword";

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

const user = createUser("Yoda", "admin", true);

type User = typeof user;

//
// This is costly. Create a user just so we can then create a
// type out of it‽
//
// What if to create a user we need to reach out to the
// network, or the database, or both‽
//
