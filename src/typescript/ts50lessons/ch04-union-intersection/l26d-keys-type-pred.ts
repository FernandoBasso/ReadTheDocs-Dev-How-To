export const NAME = "l26d Keys and Type Predicates — type predicates";

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

type UserEventCategory = keyof UserEvents;

//
// Type predicates are a way to add more information to
// control flow analysis.
//
// What makes the type predicate work is the ‘is some type’
// in the return type annotation.
//
// Note that ‘category’ parameter is still ‘string’.
//
// Type predicates are functions that return a boolean. If
// true, the type is narrowed down to the annotated return
// type, otherwise, it is not narrowed down.
//
function isUserEventsListCategory(
  list: UserEvents,
  category: string,
): category is keyof UserEvents {
  return Object.keys(list).includes(category);
}

function filterUserEvent(
  userEventList: UserEvents,
  category: string, // UserEventCategory,
  filterKind?: EventKind
) {
  if (isUserEventsListCategory(userEventList, category)) {
    //
    // Here, ‘category’ is not ‘string’ any longer, but rather
    // ‘keyof UserEvents’, which means it is certainly one of
    // watching, rsvp, attended or signedout.
    //
    const filteredList = userEventList[category];

    if (filterKind) {
      return filteredList.filter((event) => event.kind === filterKind);
    }

    return filteredList;
  }

  return userEventList;
}

