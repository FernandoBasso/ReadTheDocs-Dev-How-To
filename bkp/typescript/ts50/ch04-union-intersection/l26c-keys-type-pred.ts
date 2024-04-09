export const NAME = "l26c Keys and Type Predicates — type predicates";

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
// This function is almost a type predicate (type guard), but
// not quite yet...
//
function isUserEventsListCategory(
  list: UserEvents,
  category: string,
) {
  return Object.keys(list).includes(category);
}

function filterUserEvent(
  userEventList: UserEvents,
  category: string, // UserEventCategory,
  filterKind?: EventKind
) {
  if (isUserEventsListCategory(userEventList, category)) {
    const filteredList = userEventList[category];

    if (filterKind) {
      return filteredList.filter((event) => event.kind === filterKind);
    }

    return filteredList;
  }

  return userEventList;
}

//
// This implementation provides enought safety so that the
// program doesn't crash, but we can avoid the type warnings and
// and errors and do a better work in general, and get the
// connection between ‘UserEvents’ and ‘category’ types back.
//

