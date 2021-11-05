import React from "react";
import { shallow } from "enzyme";
import Bookstore from "./Bookstore";

describe("Bookstore", () => {
  test("matches snapshot", () => {
    const wrapper = shallow(<Bookstore />);
    expect(wrapper).toMatchSnapshot();
  });
});
