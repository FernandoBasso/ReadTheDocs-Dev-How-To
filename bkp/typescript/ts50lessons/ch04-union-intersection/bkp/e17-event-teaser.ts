export const NAME = "e17 event tracking";

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

function neverError(message: string, token: never): Error {
  return new Error(`${message}. ${token} should not exist.`);
}

function filterUserEvent(
  list: UserEvents,
  category: string,
  filterKind?: EventKind
) {
  if (isUserEventListCategory(list, category)) {
    const filteredList = list[category];

    if (filterKind) {
      return filteredList.filter((event) => event.kind === filterKind);
    }

    return filteredList;
  }

  return list;
}

function getEventTeaser(event: TechEvent): string {
  switch(event.kind) {
    case "conference":
      return `${event.title} (Conference), ` +
        `priced at ${event.price} USD`;
    case "meetup":
      return `${event.title} (Meetup), ` +
        `hosted at ${event.location}`;
    case "webinar":
      return "${event.title} (Webinar), " +
        `available online at ${event.url}`
    default:
      throw neverError(
        "Not sure what to do with that",
        event, // <1> ‘hackathon’ instead of ‘never’.
      );
  }
}
//
// ‘event’ should be ‘never’, but it is ‘hackathon’ because
// the type checker verifies we have not handled it yet.
//
// By using the ‘neverError()’ function, we have to pass the
// ‘event’, and that helps us catch potential failures in
// handling all the case possibilities.
//
// Using ‘never’ with a function like ‘neverError()’ is a good
// means to safeguard against situations that could occur, but
// shouldn't.
//
