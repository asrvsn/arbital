class Argument {

  constructor({summary, body, owner, creationDate, claimLinks, id}) {
    this._summary = summary
    this._body = body
    this._owner = owner
    this._creationDate = creationDate
    this._claimLinks = claimLinks
    this._id = id
  }

  get summary() {
    return this._summary
  }

  get body() {
    return this._body
  }

  get owner() {
    return this._owner
  }

  get creationDate() {
    return this._creationDate
  }

  get claimLinks() {
    return this._claimLinks
  }

  get id() {
    return this._id
  }

  static fromJSON(obj) {
    return new Argument({
      summary: obj.argumentSummary,
      body: obj.argumentBody,
      owner: obj.argumentOwner,
      creationDate: Date.parse(obj.argumentCreationDate),
      id: obj.argumentId
    })
  }

  toJSON() {
    return {
      argumentSummary: this.summary,
      argumentBody: this.body,
      argumentOwner: this.owner,
      argumentCreationDate: this.creationDate.toString(),
      argumentId: this.id
    }
  }
}

export default class Argument
