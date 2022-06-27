export const NAME = "l26a Keys and Type Predicates";

type TechEventBase = {
  title: string;
  description: string;
  date: Date;
  capacity: number;
  rsvp: number;
};

type Talk = {
  title: string;
  abstract: string;
  speaker: string;
};

type Conference = TechEventBase & {
  location: string;
  price: number;
  talks: Talk[];
  kind: "conference";
};

type Meetup = TechEventBase & {
  location: string;
  price: string;
  talks: Talk[];
  kind: "meetup";
};

type Webinar = TechEventBase & {
  url: string;
  price?: number;
  talks: Talk;
  kind: "webinar";
};

type Hackathon = TechEventBase & {
  location: string;
  price?: number;
  kind: "hackathon";
};

type TechEvent = Webinar | Conference | Meetup | Hackathon;

type EventKind = TechEvent["kind"];

//
// We are doing this...
//
type UserEvents = {
  watching: TechEvent[];
  rvsp: TechEvent[];
  attended: TechEvent[];
  signedout: TechEvent[];
};

//
// ...and have to match this union manually keeping it in
// sync with ‘UserEvents’. More work and error-prone.
//
// By the way, did you see the mistake “signedout” vs
// “signedoff”‽
//
// We should generally create types like this dynamically.
//
type UserEventCategory = "watching" | "rsvp" | "attended" | "signedoff";

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

declare const events: UserEvents;

//
// There is no "signedoff" in ‘UserEvents’.
//
filterUserEvent(events, "signedoff", "hackathon");
