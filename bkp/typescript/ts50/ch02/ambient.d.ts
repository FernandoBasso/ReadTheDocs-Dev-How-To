export {};

// declare const isDev: boolean;

declare global {
  interface Window {
    isDev: boolean;
  }
}
