export const NAME = "fn-obj";

type AssembleFn = (includeTags: boolean) => string;

type Query = {
  query: string;
  tags?: string[];
  assemble: AssembleFn;
};

//
// TODO: Turn this into a functional style. Remove ‘query’
// reassignment, for example, and compose stuff.
//
const query: Query = {
  query: "Ember",
  tags: ["javascript"],
  assemble(includeTags = false): string {
    let query = `?query=${this.query}`;

    if (includeTags && typeof this.tags !== "undefined") {
      query += `&${this.tags.join(",")}`;
    }
    return query;
  },
};


//
// vim: set textwidth=68:
//
