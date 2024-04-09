export const NAME = "l27b Bottom - never";

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

function neverError(
  message: string,
  token: never // The culprit.
) {
  return new Error(`${message}. ${token} should not exist`);
}

function getEventTeaser(event: TechEvent): string | never {
  switch (event.kind) {
    case "conference":
      return `${event.title} (Conference), priced at ${event.price} USD`;
    case "meetup":
      return `${event.title} (Meetup), hosted at ${event.location}`;
    case "webinar":
      return `${event.title} (Webinar), available online at ${event.url}`;

    //
    // Comment and uncomment the two lines bellow and see what
    // happens to ‘neverError()’ in the default branch.
    //
    case "hackathon":
      return `${event.title} (Hackathon)`;
    default:
      //
      // Because we haven't handled ‘hackathon’, ‘event.kind’
      // is not yet ‘never’, and ‘neverError()’ takes a never,
      // therefore warning us that we haven't handled all
      // possible cases.:
      //
      // Using a function similar to ‘neverError()’ is a good
      // way to make sure we don't forget to handle cases in
      // situations like this.
      //
      neverError("What the poop‽", event.kind);
  }
}

//
// But above, if we forget to handle a case, we would through
// an error even if we still have valid event kinds to deal
// with.
//
