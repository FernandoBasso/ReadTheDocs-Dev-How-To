import { spy, assertSpyCalls } from "/deps.ts";
import { logAtLeast5 } from "./logAtLeast5-v1.ts";

Deno.test("logAtLeast5()", async (t) => {
  await t.step("should call log 5 times", () => {
    const spyLog = spy(console, "log");

    logAtLeast5(1);

    assertSpyCalls(spyLog, 5);

    spyLog.restore();
  });

  await t.step("should log 7 times", () => {
    const spyLog = spy(console, "log");

    logAtLeast5(7);

    assertSpyCalls(spyLog, 7);

    spyLog.restore();
  });
});
