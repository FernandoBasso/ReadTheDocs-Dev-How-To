import {
  round,
  gradeStudents,
} from './grade_v2';

describe('round()', () => {
  describe('when the difference is NOT less than 3', () => {
    it('should not round up', () => {
      // 95 - 90 = 5
      expect(round(90)).toEqual(90);

      // 75 - 71 = 4
      expect(round(71)).toEqual(71);

      // 45 - 42 = 3
      expect(round(42)).toEqual(42);

      // 100 - 96 = 4
      expect(round(96)).toEqual(96);

      // 100 - 97 = 3
      expect(round(97)).toEqual(97);
    });
  });

  describe('when the difference IS less than 3', () => {
    it('should round up', () => {
      // 45 - 43 = 2, then round up!
      expect(round(43)).toEqual(45);

      // 45 - 44 = 1, then round up!
      expect(round(44)).toEqual(45);

      // 50 - 48 = 2, then round up!
      expect(round(48)).toEqual(50);

      // 50 - 49 = 1, then round up!
      expect(round(49)).toEqual(50);
    });
  });
});

describe('Grading Students', () => {
  describe('when the grade is < 38', () => {
    it('should return the number without any rounding', () => {
      expect(
        gradeStudents([0, 13, 28, 37])
      ).toEqual([0, 13, 28, 37]);
    });
  });

  describe('when the grade is >= 38', () => {
    it('should return 42 and not round to 45', () => {
      expect(
        gradeStudents([38, 77, 78, 100])
      ).toEqual([40, 77, 80, 100]);
    });
  });

  describe('when there are failing and passing grades', () => {
    it('should return the correct list of grades', () => {
      expect(
        gradeStudents([0, 17, 18, 37, 38, 39, 40, 50, 96, 88, 100])
      ).toEqual([0, 17, 18, 37, 40, 40, 40, 50, 96, 90, 100]);
    });
  });
});
