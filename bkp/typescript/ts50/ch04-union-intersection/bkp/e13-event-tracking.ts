export const NAME = "e13 event tracking";

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
// 2. ...we would also need to update this one (and
// vice-versa).
//
// type UserEventCategory =
//   "watching" | "rsvp" | "attended" | "signedout";
//

function filterUserEvent(
  userEventList: UserEvents,
  category: keyof UserEvents, // 🚀 We do this instead! 🚀
  filterKind?: EventKind
): TechEvent[] {
  const filteredList = userEventList[category];

  if (filterKind) {
    return filteredList.filter((event) => event.kind === filterKind);
  }

  return filteredList;
}

//
// Check this:
//
// • https://www.notion.so/devhowto/keyof-80fe169c2ecf437a9b40cea65ae4b1e9
//
