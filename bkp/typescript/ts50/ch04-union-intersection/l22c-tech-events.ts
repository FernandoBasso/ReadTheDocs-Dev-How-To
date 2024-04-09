export const NAME = "l22c Modeling Data - Union Types";

const log: Console["log"] = console.log.bind(console);

type TechEventBase = {
  title: string;
  description: string;
  date: Date;
  capacity: number;
  rsvp: number;
  kind: string;
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
};

type Meetup = TechEventBase & {
  location: string;
  price: string;
  talks: Talk[];
};

type Webinar = TechEventBase & {
  url: string;
  price?: number;
  talks: Talk;
};

//
// Using Union Types to create a ‘TechEvent’ that can be
// either a ‘Webinar’, a ‘Conference’ or a ‘Meetup’.
//
// Read ‘|’ as the “or” operator.
//
type TechEvent = Webinar | Conference | Meetup;

//
// Cannot blindly try to access ‘location’ or ‘url’, for
// example, because they are not common to all of the
// constituents of the union.
//
declare const tev1: TechEvent;
//
// Try tev1.<Ctrl+Space>
//
//
// Also, cannot blindly assume ‘price’ is either a number or a
// string. We need some type checks to know what we've got.
//

function printEvent(event: TechEvent) {
  if (event.price) {
    //
    // Price exists!
    //
    if (typeof event.price === "number") {
      //
      // We know that price is a number
      //
      log("Price in EUR: ", event.price);
    } else {
      //
      // We know that price is a string, so the event is free!
      //
      log("It is free!");
    }
  }
  if (Array.isArray(event.talks)) {
    //
    // Talks is an array.
    //
    event.talks.forEach((talk) => {
      log(talk.title);
    });
  } else {
    //
    // It's just a single talk.
    //
    log(event.talks.title);
  }
}

//
// Note how we make use of type guards to narrow down union
// types and safely work with the values.
//
