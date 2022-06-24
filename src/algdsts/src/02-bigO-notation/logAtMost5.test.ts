import { spy, assertSpyCalls } from "/deps.ts";
import { logAtMost5 } from "./logAtMost5-v1.ts";

Deno.test("logAtMost5()", async (t) => {
  await t.step("should call log 2 times", () => {
    const spyLog = spy(console, "log");

    logAtMost5(2);

    assertSpyCalls(spyLog, 2);

    spyLog.restore();
  });

  await t.step("should log 5 times", () => {
    const spyLog = spy(console, "log");

    logAtMost5(7);

    assertSpyCalls(spyLog, 5);

    spyLog.restore();
  });
});
