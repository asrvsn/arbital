import React, { Component } from 'react'

import {Card} from 'material-ui/Card';
import TextField from 'material-ui/TextField';
import {List, ListItem} from 'material-ui/List';
import DropDownMenu from 'material-ui/DropDownMenu';
import MenuItem from 'material-ui/MenuItem';
import Divider from 'material-ui/Divider';

import backend from '../util/backend'
import AuthoredListItem from './AuthoredListItem'
import AuthenticatedHOC from '../hoc/AuthenticatedHOC'

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
    const { searchType } = this.props
    if (searchType) {
      this.setSearchType(searchType)
    }
  }

  render() {
    const { mkResult } = this.props
    const { dynSearchType, searchText, results } = this.state

    return (
      <Card>

        <DropDownMenu
          value={dynSearchType}
          onChange={(e, i, v) => this.setSearchType(v)}
        >
          <MenuItem value={1} primaryText="claims" />
          <MenuItem value={2} primaryText="arguments" />
          <MenuItem value={2} primaryText="users" />
        </DropDownMenu>
        <TextField
          hintText="search..."
          vale={searchText}
          onChange={e => this.onChange(e)}
        />

        <Divider />

        <List>
          { (results.length > 0) ?
              results.map(result => {
                const {text, userId, userName} = mkResult(result)
                return (
                  <AuthoredListItem
                    text={text}
                    authorId={userId}
                    authorName={userName}
                    onTouchTap={e => this.selectResult(result)}
                  />
                )
              })
            :
              <ListItem primaryText="Hit enter to search" />
          }
        </List>

      </Card>
    )
  }

  setSearchType(dynSearchType) {
    this.setState({dynSearchType})
  }

  onChange(e) {
    debugger
  }

  reloadSearch() {
    const { session } = this.props
    const { dynSearchType, searchText } = this.state
    const url = `/${dynSearchType}/search/${searchText}`

    backend
      .authenticate(session.id)
      .get(url, (err, response, body) => {
        if (err !== null) {
          throw err
        } else {
          if (response.statusCode == 200) {
            this.setState({results: body})
          } else {
            throw response.statusMessage
          }
        }
      })
  }

  selectResult(result) {
    const { onRequestClose } = this.props
    onRequestClose(result)
  }
}

export default AuthenticatedHOC(Finder)
