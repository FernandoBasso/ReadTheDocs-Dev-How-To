export const NAME = "l27a Bottom - never";

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

function getEventTeaser(event: TechEvent): string | never {
  switch (event.kind) {
    case "conference":
      return `${event.title} (Conference), priced at ${event.price} USD`;
    case "meetup":
      return `${event.title} (Meetup), hosted at ${event.location}`;
    case "webinar":
      return `${event.title} (Webinar), available online at ${event.url}`;
    // case "hackathon":
    //   return `${event.title} (Hackathon)`;
    default:
      //
      // ‘event.type’ is ‘hackaton’ here, but if we uncomment
      // the case above, then all cases have been exhausted,
      // and ‘event.kind’ would be ‘never’ here.
      //
      log(event.kind);
      throw new Error("Not sure what to do with that!");
  }
}

//
// But above, if we forget to handle a case, we would through
// an error even if we still have valid event kinds to deal
// with.
//
