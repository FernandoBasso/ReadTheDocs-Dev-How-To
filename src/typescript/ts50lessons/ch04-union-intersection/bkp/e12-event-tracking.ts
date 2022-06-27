export const NAME = "e12 event tracking";

const log: Console["log"] = console.log.bind(console);

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
// 2. ...we would also need to update this one (and
// vice-versa).
//
type UserEventCategory =
  "watching" | "rsvp" | "attended" | "signedout";

function filterUserEvent(
  userEventList: UserEvents,
  category: UserEventCategory,
  filterKind?: EventKind
) {
  const filteredList = userEventList[category];

  if (filterKind) {
    return filteredList.filter((event) => event.kind === filterKind);
  }

  return filteredList;
}

