export const NAME = "l32c Modeling Data";

const log: Console["log"] = console.log.bind(console);

//
// The ‘&’ means we are making an intersection between
// ‘TechEventBase’ and the literal type on the right.
//
// The ‘|’ means we are creating a union type.
//

type Talk = {
  title: string;
  abstract: string;
  speaker: string;
};

type TechEventBase = {
  title: string;
  description: string;
  date: Date,
  capacity: number;
  rsvp: number;
  kind: string;
}

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
  price?: string;
  talks: Talk;
};

type TechEvent = Conference | Meetup | Webinar;

//
// There is type narrowing going on here. Our conditions act like
// type guards to narrow down types.
//
function printEvent(event: TechEvent) {
  if (event.price) {
    //
    // Price exists!
    //
    if (typeof event.price === 'number') {
      //
      // We know that price is a number
      //
      log('Price in EUR: ', event.price);
    } else {
      //
      // We know that price is a string, so the
      // event is free!
      //
      log('It is free!');
    }
  }
  if (Array.isArray(event.talks)) {
    //
    // Talks is an array. The type guard guarantees that.
    //
    event.talks.forEach(talk => {
      log(talk.title);
    })
  } else {
    //
    // It's just a single talk. If not an array, then
    // it must be a single talk.
    //
    log(event.talks.title);
  }
}
