const example = require('../../../../examples/simple-dominion-tracker.json');

describe('link functionality', () => {
  beforeEach(() => {
    cy.visit('http://localhost:8000/src/Main.elm')
  });

  describe('the edit link', () => {
    it('brings you back to the same schema', () => {
      const original = JSON.stringify(example);
      cy.get('textarea').type(original, { parseSpecialCharSequences: false, delay: 0 });

      cy.get('a.edit-link').then(v => {
        cy.visit(v[0].href);
        cy.get('textarea').should('have.value', original);
      });
    });
  });
});
