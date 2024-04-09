export const NAME = "l28a undefined and null";

//
// Some of the things spoken about in this lesson depend on
// the settings in ‘tsconfig.json’, like ‘"strict": true’,
// more specifically, ‘strictNullChecks’. Without it,
// ‘undefined’ and ‘null’ are part of all types. With it, they
// are not part of any types except their own value types.
//

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

function getTeaserHTML(event: TechEvent) {
  return `<h2>${event.title}</h2>
          <p>${event.description}</p>`;
}

function getTeaserListElement(event: TechEvent) {
  const content = getTeaserHTML(event);
  const element = document.createElement("li");

  element.classList.add("teaser-card");
  element.innerHTML = content;

  return element;
}

function appendEventToList(event: TechEvent) {
  const list = document.querySelector("#event-list");
  const element = getTeaserListElement(event);

  //
  // Yeah, the element with the id ‘event-list’ may not be
  // present in the DOM.
  //
  list.append(element);
}
