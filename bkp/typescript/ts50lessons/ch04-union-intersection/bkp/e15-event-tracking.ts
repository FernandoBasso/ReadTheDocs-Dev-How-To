export const NAME = "e15 event teaser";

const log: Console["log"] = console.log.bind(console);

//
// keyof, type assertions (a.k.a. type guards).
//

type Talk = {
  title: string;
  abstract: string;
  speaker: string;
};

type TechEventBase = {
  title: string;
  description: string;
  date: Date;
  capacity: number;
  rsvp: number;
};

type Conference = TechEventBase & {
  kind: "conference";
  location: string;
  price: number;
  talks: Talk[];
};

type Meetup = TechEventBase & {
  kind: "meetup";
  location: string;
  price: string;
  talks: Talk[];
};

type Webinar = TechEventBase & {
  kind: "webinar";
  url: string;
  price?: string;
  talks: Talk; // Why plural in the book?
};

type Hackaton = TechEventBase & {
  kind: "hackathon";
  url: string;
  price?: number;
};

type TechEvent = Conference | Meetup | Webinar | Hackaton;

type EventKind = TechEvent["kind"];

//
// 1. Should we update this type...
//
type UserEvents = {
  watching: TechEvent[];
  rsvp: TechEvent[];
  attended: TechEvent[];
  signedout: TechEvent[];
};

//
// This is checking for the existence of a certain category,
// but it is not a full type guard/assertion yet.
//
function isUserEventListCategory(
  list: UserEvents,
  category: string,
): category is keyof UserEvents { // <1>
  //
  // If this function evaluates to true, we can be
  // sure ‘category’ is is a key of ‘UserEvents’.
  //
  return Object.keys(list).includes(category);
}
//
// <1> We turned this function into a proper type guard.
//

function getEventTeaser(event: TechEvent): string | never {
  switch(event.kind) {
    case "conference":
      return `${event.title} (Conference), ` +
        `priced at ${event.price} USD`;
    case "meetup":
      return `${event.title} (Meetup), ` +
       `hosted at ${event.location}`
    // case "webinar":
    //   return `${event.title} (Webinar), ` +
    //   `available online at ${event.url}`
    // case "hackathon":
    //   return `${event.title} (Hackathon)`
    default:
      //
      // 👀👀👀👀👀👀👀👀👀👀👀
      // 👀 THIS IS IMPORTANT 👀
      // 👀👀👀👀👀👀👀👀👀👀👀
      //
      // If we handle all cases, ‘event’ is ‘never’.
      // If we comment one or two of the above cases,
      // ‘event’ is one of the above.
      //
      event // webinar | hackaton.
      throw new Error("Not sure what to do with that!")
  }
}


declare var tev: TechEvent;
var result = getEventTeaser(tev);
//
// NOTE that result is ‘string’, and not
// ‘string | never’. We will never have result be
// assigned anything if we throw an exception at the
// default case for the switch.
//

