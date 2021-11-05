import React, { Component } from 'react';
import axios from 'axios';
import Table from 'react-bootstrap/Table'
import 'bootstrap/dist/css/bootstrap.min.css';

class Bookstore extends Component {
  state = {
    books: [],

  };
  constructor(props) {
    super(props);

  }


  componentDidMount() {
    axios.get(`http://localhost:8080/books`)
      .then(res => {
        const books = res.data;
        this.setState({ books: books });
      })
  }
  render() {
    return (
      <>
        <Table striped bordered hover>
          <thead>
            <tr>
              <th>ISBN</th>
              <th>Title</th>
            </tr>
          </thead>
          <tbody>
            {this.state.books.map(book => <tr><td>{book.isbn}</td><td>{book.book}</td></tr>)}
          </tbody>
        </Table>
      </>
    );
  }
}

export default Bookstore;
