import React, { Component } from 'react'

import {Card} from 'material-ui/Card';
import TextField from 'material-ui/TextField';
import {List, ListItem} from 'material-ui/List';
import DropDownMenu from 'material-ui/DropDownMenu';
import MenuItem from 'material-ui/MenuItem';
import Divider from 'material-ui/Divider';

import backend from '../util/backend'
import AuthoredListItem from './items/AuthoredListItem'
import AuthenticatedHOC from './hoc/AuthenticatedHOC'

const styles = {
  firstRow: {
    display: 'flex',
    flexDirection: 'row',
    alignItems: 'center'
  },
  dropdown: {
    marginTop: -4
  },
  input: {
    marginLeft: 10,
    marginRight: 5
  }
}

class Finder extends Component {
  constructor(props) {
    super(props)
    this.state = {
      searchText: "",
      dynSearchType: "claims",
      results: [],
    }
  }

  componentDidMount() {
    const { mode } = this.props
    if (!! mode) {
      this.setSearchType(mode)
    }
  }

  render() {
    const { mode } = this.props
    const { dynSearchType, searchText, results } = this.state
    const mkResult = (result) => {
      switch(dynSearchType) {
        case 'claims': {
          const { text, authorId, authorName } = result
          return {text, userId: authorId, userName: authorName}
        }
        case 'arguments': {
          const { text, authorId, authorName } = result
          return {text, userId: authorId, userName: authorName}
        }
        case 'users': {
          const { email, id, name } = result
          return {text: email, userId: id, userName: name}
        }
      }
    }

    return (
      <Card>

        <div style={styles.firstRow}>

          <TextField
            hintText="search..."
            value={searchText}
            onChange={e => this.onChange(e)}
            onKeyDown={e => this.onKeyDown(e)}
            style={styles.input}
          />

          { (mode === undefined) &&
            <DropDownMenu
              value={dynSearchType}
              onChange={(e, i, v) => this.setSearchType(v)}
              labelStyle={styles.dropdown}
              iconStyle={styles.dropdown}
            >
              <MenuItem value={"claims"} primaryText="claims" />
              <MenuItem value={"arguments"} primaryText="arguments" />
              <MenuItem value={"users"} primaryText="users" />
            </DropDownMenu>
          }

        </div>

        <Divider />

        <List>
          { (results.length > 0) ?
              results.map(result => {
                const {text, userId, userName} = mkResult(result)
                return (
                  <AuthoredListItem
                    key={result.id}
                    text={text}
                    authorId={userId}
                    authorName={userName}
                    onTouchTap={e => this.selectResult(result)}
                  />
                )
              })
            :
              <ListItem primaryText="(nothing) hit enter to search" />
          }
        </List>

      </Card>
    )
  }

  setSearchType(dynSearchType) {
    this.setState({dynSearchType})
  }

  onChange(e) {
    this.setState({ searchText: e.target.value })
  }

  onKeyDown(e) {
    if (e.key === 'Enter') {
      this.reloadSearch()
    }
  }

  reloadSearch() {
    const { session } = this.props
    const { dynSearchType, searchText } = this.state

    if (searchText === "") {
      this.setState({ results: [] })
    } else {
      const url = `/${dynSearchType}/search/${searchText}`

      backend
        .authenticate(session.id)
        .get(url, (err, response, body) => {
          if (err !== null) {
            throw err
          } else {
            if (response.statusCode == 200) {
              this.setState({ results: JSON.parse(body) })
            } else {
              throw response.statusMessage
            }
          }
        })
    }
  }

  selectResult(result) {
    const { onRequestClose } = this.props
    const { dynSearchType } = this.state
    result.type = dynSearchType
    onRequestClose(result)
  }
}

export default AuthenticatedHOC(Finder)
