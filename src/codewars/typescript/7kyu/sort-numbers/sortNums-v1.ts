
function sortAscending(x: number, y: number): number {
  return x - y;
}

export function sortNums(xs?: number[] | null | undefined): number[] {
  if (xs === undefined || xs === null) return [];

  return xs.sort(sortAscending);
}
