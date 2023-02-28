const assertValid = () => {
  cy.get('.definition-valid').should('exist')
  cy.get('.definition-invalid').should('not.exist')
};

const assertInvalid = () => {
  cy.get('.definition-valid').should('not.exist')
  cy.get('.definition-invalid').should('exist')
};

const assertInvalidWith = (selectors) => {
  cy.get('.definition-valid').should('not.exist')
  cy.get('.definition-invalid').should('exist')
  selectors.forEach(selector =>
    cy.get(selector).should('exist')
  );
};

const shell = tracker => ({
  name: "T",
  tracker
});

const exampleNumberField = f => shell(f({
  "type": "number",
  "text": "Actions",
  "default": 1,
  "id": "a"
}))


const exampleTextField = shell({
  "type": "text",
  "text": "Enter something",
  "id": "something"
});

describe('parsing tracker schemas', () => {
  beforeEach(() => {
    cy.visit('http://localhost:8000/src/Main.elm')
  });

  it('does not accept random garbage', () => {
    cy.get('textarea').type('Random garbage LOL')
    assertInvalid();
  });

  it('expects valid JSON', () => {
      cy.get('textarea').type(JSON.stringify(exampleTextField) + 'BAD', { parseSpecialCharSequences: false, delay: 0 })
    assertInvalid();
  });

  describe('top-level', () => {
    it('requires a name', () => {
      const withoutName = { ...exampleTextField };
      delete withoutName.name;
      cy.get('textarea').type(JSON.stringify(withoutName), { parseSpecialCharSequences: false, delay: 0 })
      assertInvalid();
    });

    it('allows for the hiding of "turn" and "game" based language', () => {
       const withoutTurns = { ...exampleTextField, turns: false };
       cy.get('textarea').type(JSON.stringify(withoutTurns), { parseSpecialCharSequences: false, delay: 0 })
       assertValid();
    });
  });

  describe('validations', () => {
    it('fails when multiple player groups are given', () => {
      cy.get('textarea').type(JSON.stringify(
      shell({
        type: 'player-group',
        minPlayers: 2,
        maxPlayers: 2,
        items: [
          {
            type: 'player-group',
            minPlayers: 2,
            maxPlayers: 2,
            items: [
            ]
          } 
        ]
      })
      ), { parseSpecialCharSequences: false, delay: 0 });
      assertInvalidWith(['#error-key-too-many-player-groups']);
    });

    it('fails when IDs are repeated', () => {
      cy.get('textarea').type(JSON.stringify(
        shell({
          type: 'group',
          items: [
            exampleNumberField(s => s).tracker,
            exampleNumberField(s => s).tracker
          ]
        })
      ), { parseSpecialCharSequences: false, delay: 0 });
      assertInvalidWith(['#error-key-duplicate-ids']);
    });

    it('fails when a player group is in an item list', () => {
      cy.get('textarea').type(JSON.stringify(
        shell({
          type: 'item-list',
          text: 'Items',
          id: 'items',
          items: [
              {
              type: 'player-group',
              minPlayers: 1,
              maxPlayers: 4,
              items: []
            }
          ]
        })
      ), { parseSpecialCharSequences: false, delay: 0 });
      assertInvalidWith(['#error-key-player-group-in-list']);
    });

    it('fails when an action tries to set the current player outside of a player group', () => {
      cy.get('textarea').type(JSON.stringify(
        shell({
          "type": "action",
          "text": "Set to this player!",
          "effects": [
            {
              "type": "set-current-player",
              "target": "this-player"
            }
          ]
        })
      ), { parseSpecialCharSequences: false, delay: 0 });
      assertInvalidWith(['#error-key-set-current-player-outside-of-group']);
    });
  });

  describe('component types', () => {
    describe('text field', () => {
      it("parses", () => {
        cy.get('textarea').type(JSON.stringify(exampleTextField), { parseSpecialCharSequences: false, delay: 0 });
        assertValid();
      });
    });

    describe('number', () => {
      [
        ['minimal fields', exampleNumberField(s => s)],
        ['decimal defaults', exampleNumberField(s => ({ ...s, default: 42.42 }))],
        ['disabled', exampleNumberField(s => ({...s, disabled: true}))],
        ['hidden', exampleNumberField(s => ({...s, hidden: true}))],
        ['playerDefaults', exampleNumberField(s => ({...s, playerDefaults: [{ player: 0, default: 42}]}))]
      ].forEach(([desc, schema]) =>
        it(`parses with ${desc}`, () => {
          cy.get('textarea').type(JSON.stringify(schema), { parseSpecialCharSequences: false, delay: 0 });
          assertValid();
        })
      );
    });

    describe('action', () => {
      it("parses for a big example", () => {
          const example = shell({
            type: 'player-group',
            minPlayers: 2,
            maxPlayers: 4,
            items: [ {
              type: "action",
              text: "A",
              effects: [
                {type:  "next-turn" },
                {type: "set-current-player", target: "this-player" },
                {type: "restore-default", targetId: "some-field", scope: 'this-player' },
                {type: "adjust", targetId: "another-field", amount: -1, scope: 'all-players' },
              ]
            }
          ]
        });
        cy.get('textarea').type(JSON.stringify(example), { parseSpecialCharSequences: false, delay: 0 });
        assertValid();
      });
    });

    describe('group', () => {
      it("parses a simple group", () => {
        const example = shell({
          type: "group",
          items: [
              exampleTextField.tracker,
              exampleNumberField(s => s).tracker
          ]
        });
        cy.get('textarea').type(JSON.stringify(example), { parseSpecialCharSequences: false, delay: 0 });
        assertValid();
      });

      it("parses nested groups", () => {
        const example = shell({
          type: "group",
          items: [
              {
                  type: "group",
                  items: []
              }
          ]
        });
        cy.get('textarea').type(JSON.stringify(example), { parseSpecialCharSequences: false, delay: 0 });
        assertValid();
      });
    });

    describe('player-group', () => {
      it("parses", () => {
        const example = shell({
          type: "player-group",
          minPlayers: 2,
          maxPlayers: 4,
          defaultAliases: ["Bob", "Jane"],
          items: [
            exampleTextField.tracker,
            exampleNumberField(s => s).tracker
          ]
        });
        cy.get('textarea').type(JSON.stringify(example), { parseSpecialCharSequences: false, delay: 0 });
        assertValid();
      });
    });

    describe('item-list', () => {
      it("parses", () => {
        const example = shell({
          type: "item-list",
          text: 'IL',
          id: 'il',
          items: [
            exampleTextField.tracker,
            exampleNumberField(s => s).tracker
          ]
        });
        cy.get('textarea').type(JSON.stringify(example), { parseSpecialCharSequences: false, delay: 0 });
        assertValid();
      });
    });

    describe('calculated', () => {
      it("parses a large example", () => {
        const example = shell(
          {
            type: "player-group",
            minPlayers: 2,
            maxPlayers: 2,
            items: [
              {
                type: "calculated",
                text: ':)',
                id: 'some-id',
                equals: {
                  "type": "add",
                  "ops": [
                    { "type": "ref", "targetId": "target", "scope": "all-players" },
                    {type: "mul", ops: [ {"type": "literal", "value": 42.99}, {"type": "literal", "value": 42.99} ] },
                    {"type": "sum", "of": { "type": "ref", "targetId": "target" }}
                  ]
                }
              } 
            ]
          }
          );
        cy.get('textarea').type(JSON.stringify(example), { parseSpecialCharSequences: false, delay: 0 });
        assertValid();
      });
    });
  });

  describe('examples', () =>
    [
      'calculated-referencing-calculated.json',
      'contrived-multi-player-dominion-tracker.json',
      'defaults-support-floats.json',
      'hidden-fields.json',
      'kill-team-tracker.json',
      'known-error.json',
      'money.json',
      'per-player-defaults.json',
      'simple-dominion-tracker.json',
    ].forEach(example =>
      it(`parses the example ${example}`, () => {
        cy.get('textarea').type(JSON.stringify(require(`../../../../examples/${example}`)), { parseSpecialCharSequences: false, delay: 0 });
        assertValid();
      })
    )
  );
});
