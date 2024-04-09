export {};

const log: Console["log"] = console.log.bind(console);

type AppContext = {
  brand: "A1" | "B2" | "C3";
  brandtype: "a1brand" | "b2brand" | "c3brand";
  locale: "es_MX" | "en_US" | "hi_IN";
};

type BrandType = AppContext["brandtype"];

type URLs = { [key in BrandType]: string };

const urls: URLs = {
  a1brand: "https://a1.example.com/api",
  b2brand: "https://b2.example.com/api",
  c3brand: "https://c3.example.com/api",
};

function getApiUrl(appCtx: AppContext): string {
  return urls[appCtx.brandtype];
}

function assertEquals(
  actual: unknown,
  expected: unknown,
): boolean | never {
  if (expected !== actual)
    throw new Error(`‘${actual}’ does not equal ‘${expected}’.`);

  return true;
}

const mockBrandType = {
  brandType: "b2brand",
  brand: "C3",
  locale: "hi_IN",
} as unknown as AppContext;

const expectedUrl = "https://b2.example.com/api";

assertEquals(getApiUrl(mockBrandType), expectedUrl);

assertEquals(
  getApiUrl({ brandType: "c3brand" } as unknown as AppContext),
  "https://c3.example.com/api",
);

log("All tests passed!");
