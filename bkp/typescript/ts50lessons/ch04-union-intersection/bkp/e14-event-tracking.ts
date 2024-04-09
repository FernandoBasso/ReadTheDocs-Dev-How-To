export const NAME = "e14 event tracking";

const log: Console["log"] = console.log.bind(console);

//
// keyof
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
) {
  return Object.keys(list).includes(category);
}

//
// ‘category’ is a simple string now.
//
function filterUserEvent(
  list: UserEvents,
  category: string,
  filterKind?: EventKind
) {
  if (isUserEventListCategory(list, category)) {
    const filteredList = userEventList[category];

    if (filterKind) {
      return filteredList.filter((event) => event.kind === filterKind);
    }

    return filteredList;
  }

  return list;
}

//
// This is correct regarding runtime checks, but the type
// checker is unhappy.
//
